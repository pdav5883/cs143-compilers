#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

//////////////////////////////////////////////////////////
////
//// ClassTableEntry
////
////////////////////////////////////////////////////////// *
ClassTableEntry::ClassTableEntry(Class_ class_node) {
  name = class_node->get_name();
  node = class_node;
  osymtab = new SymbolTable<Symbol, Entry>();
  msymtab = new SymbolTable<Symbol, Signature>();
}

//////////////////////////////////////////////////////////
////
//// ClassTable
////
//////////////////////////////////////////////////////////
ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) , tbl(NULL) {

  // add the base classes with node skeletons to the table
  install_basic_classes();

  // iterate through list of class nodes, adding each to the table after checking no redefine
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ class_node = classes->nth(i);
    Symbol class_name = class_node->get_name();
    
    // Cannot redefine an existing class
    if (entry_exists(class_name)) {
      semant_error(class_node) << "Semant Abort: Illegal redefinition of class " << class_name << endl;
      cerr << "Compilation halted due to static semantic errors." << endl;
      exit(1);
    }

    // Cannot redefine SELF_TYPE
    if (class_name == SELF_TYPE) {
      semant_error(class_node) << "Semant Abort: Cannot redefine SELF_TYPE" << endl;
      cerr << "Compilation halted due to static semantic errors." << endl;
      exit(1);
    }
    add_entry(class_node);
  }

  // Make sure we have a main class
  if (!entry_exists(Main)) {
    semant_error() << "Class Main is not defined." << endl;
  }

  // climb up inheritance graph for each class: check for cycles, missing classes, or illegal inherits
  for (List<ClassTableEntry> *curr = tbl; curr != NULL; curr = curr->tl()) {
    Symbol start_name = curr->hd()->get_name();
    Symbol curr_name = start_name;

    while(curr_name != Object) {
      curr_name = get_parent_name(curr_name);

      if (curr_name == start_name) { 
	semant_error(curr->hd()->get_node()) << "Semant Abort: Circular inheritance for class " << start_name << endl;
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
      }
      else if (curr_name == Int || curr_name == Str || curr_name == Bool) {
	semant_error(curr->hd()->get_node()) << "Semant Abort: Class " << start_name << " inherits from Int/Str/Bool" << endl;
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
      }
      else if (!entry_exists(curr_name)) {
	semant_error(curr->hd()->get_node()) << "Semant Abort: Class " << start_name << " inherits undefined class " 
					     << curr_name << endl;
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
      }
    }
  }
  
  

  // build the method and attribute symbol tables for each class, checking for (recoverable) errors in the process
  for (List<ClassTableEntry> *curr = tbl; curr != NULL; curr = curr->tl()) {
    build_symtabs(curr->hd()->get_name(), curr->hd()->get_osymtab(), curr->hd()->get_msymtab());
  }
}

ClassTableEntry* ClassTable::get_entry(Symbol class_name) {
  for (List<ClassTableEntry> *curr = tbl; curr != NULL; curr = curr->tl()) {
    if (curr->hd()->get_name() == class_name) {
      return curr->hd();
    }
  }
  return NULL;
}

ClassTableEntry* ClassTable::get_entry(Symbol class_name, Symbol self_class_name) {
  if (class_name == SELF_TYPE) { class_name = self_class_name; }
  return get_entry(class_name);
}

bool ClassTable::entry_exists(Symbol class_name) {
  for (List<ClassTableEntry> *curr = tbl; curr != NULL; curr = curr->tl()) {
    if (curr->hd()->get_name() == class_name) {
      return true;
    }
  }
  return false;
}

bool ClassTable::entry_exists(Symbol class_name, Symbol self_class_name) {
  if (class_name == SELF_TYPE) { class_name = self_class_name; }
  return entry_exists(class_name);
}

bool ClassTable::is_parent(Symbol c1, Symbol c2) {
  // is class c1 a parent class of c2?
  while (true) {
    if (c1 == c2) { return true; }
    else if ( c2 == Object ) { return false; }
    c2 = get_parent_name(c2);
  }
}

bool ClassTable::is_parent(Symbol c1, Symbol c2, Symbol class_name) {
  if (c1 == SELF_TYPE) { return c1 == c2; }
  if (c2 == SELF_TYPE) { c2 = class_name; }
  return is_parent(c1, c2);
}

ClassTableEntry* ClassTable::get_parent_entry(Symbol class_name) {
  Symbol parent_class_name = get_parent_name(class_name);
  return get_entry(parent_class_name);
}

Symbol ClassTable::get_parent_name(Symbol class_name) {
  ClassTableEntry *entry = get_entry(class_name);
  return entry->get_parent_name();
}

void ClassTable::add_entry(Class_ class_node) {
  if (!entry_exists(class_node->get_name()))
    { tbl = new List<ClassTableEntry>(new ClassTableEntry(class_node), tbl); }
}

Symbol ClassTable::get_common_parent(Symbol c1, Symbol c2) {
  // convenience check for finding the join of list of types
  if (c2 == NULL) { return c1; }
  while (true) {
    if (is_parent(c1, c2)) { return c1; }
    c1 = get_parent_name(c1);
  }
}

Symbol ClassTable::get_common_parent(Symbol c1, Symbol c2, Symbol class_name) {
  if (c1 == SELF_TYPE) { c1 = class_name; }
  if (c2 == SELF_TYPE) { c2 = class_name; }
  return get_common_parent(c1, c2);
}

void ClassTable::build_symtabs(Symbol class_name, SymbolTable<Symbol, Entry> *osymtab, SymbolTable<Symbol, Signature> *msymtab) {
  // call recursively until we get to the top of the class heirarchy
  if (class_name != Object) { build_symtabs(get_parent_name(class_name), osymtab, msymtab); }

  // enter a new scope as we move back down the inheritance tree
  osymtab->enterscope();
  msymtab->enterscope();

  // self is always in scope with the current class type - deleted because not contained in all type rules
  osymtab->addid(self, SELF_TYPE);

  // get the list of features from the class node, which are a mix of attributes and methods
  Class_ class_node = get_entry(class_name)->get_node();
  Features features = class_node->get_features();

  // iterate through the features, add them to correct symbol table with error checking
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature feature = features->nth(i);

    // add feature to method symbol table
    if (feature->is_method()) {
      method_class *method_feature = dynamic_cast<method_class*>(feature);
      // method in symbol table at current scope: ERROR
      if (msymtab->probe(method_feature->get_name()) != NULL) {
	semant_error(class_node->get_filename(), method_feature) << "Error: Method " << method_feature->get_name()
                                                                 << " is defined multiple times in class" << endl;
      }
      else {
	// method not yet in symbol table: OK
	if (msymtab->lookup(method_feature->get_name()) == NULL) {
	  msymtab->addid(method_feature->get_name(), method_feature->get_signature());
	}
	// method in symbol table with matching signature: OK
	else if (match_signature(method_feature->get_signature(), msymtab->lookup(method_feature->get_name()))) {
	  msymtab->addid(method_feature->get_name(), method_feature->get_signature());
	}
	// method in symbol table with new signature: ERROR
	else {
	  semant_error(class_node->get_filename(), method_feature) << "Error: Method " << method_feature->get_name()
                                                                   << " is illegally redefined with new signature" << endl;
	}
      }
    }

    // add feature to object symbol table 
    else {
      attr_class *attr_feature = dynamic_cast<attr_class*>(feature);
      // attribute exists at any scope: ERROR
      if (osymtab->lookup(attr_feature->get_name()) != NULL) {
	semant_error(class_node->get_filename(), attr_feature) << "Error: Attribute " << attr_feature->get_name()
							       << " is illegally redefined" << endl;
      }
      // new attribute: OK
      else {
	osymtab->addid(attr_feature->get_name(), attr_feature->get_type_decl());
      } 
    }
  }
}

List<Entry>* method_class::get_signature() {
  // the signature is a list of Symbols with the return type at the head of the list, then the arguments in order (TRet, T1, T2, ... , TN)

  // this ordering will put the first arg type at the end of the list, and last arg type at the head of the list
  List<Entry>* arg_types_rev = NULL;
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    arg_types_rev = new List<Entry>(formals->nth(i)->get_type_decl(), arg_types_rev);
  }

  // flip the order of arguments
  List<Entry>* arg_types = NULL;
  while (arg_types_rev != NULL) {
    arg_types = new List<Entry>(arg_types_rev->hd(), arg_types);
    arg_types_rev = arg_types_rev->tl();
  }
  
  // add the return type at the head
  return new List<Entry>(return_type, arg_types);
}

bool match_signature(Signature* sig1, Signature* sig2) {
  if (list_length(sig1) != list_length(sig2)) { return false; }

  while (sig1 != NULL) {
    if (sig1->hd() != sig2->hd()) { return false; }
    sig1 = sig1->tl();
    sig2 = sig2->tl();
  }
  return true;
}



////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 






void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    //  add base classes to ClassTable
    add_entry(Object_class);
    add_entry(IO_class);
    add_entry(Int_class);
    add_entry(Str_class);
    add_entry(Bool_class);

}


/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *ctab = new ClassTable(classes);

    /* some semantic analysis code may go here */
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
      classes->nth(i)->semant(ctab);
    }

    if (ctab->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
    else {
      //cerr << "Compilation succeeded, but suppressing full output in semant.cc program_class::semant()" << endl;
      //exit(1);
    }
}

void class__class::semant(ClassTable* ctab) {
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    features->nth(i)->semant(ctab, ctab->get_entry(name)->get_osymtab(), name, filename);
  }
}

void attr_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  // environment where SELF_TYPE maps to the class name
  Symbol type_decl_local;
  type_decl == SELF_TYPE ? type_decl_local = class_name : type_decl_local = type_decl;

  // check that type_decl is a defined type
  if (!ctab->entry_exists(type_decl_local)) {
    ctab->semant_error(filename, this) << "Error: Attr type " << type_decl_local << " is not defined" << endl;
  }
  // set type of init expression after temporarily adding self to the available IDs
  osymtab->enterscope();
  osymtab->addid(self, SELF_TYPE);
  init->semant(ctab, osymtab, class_name, filename);
  osymtab->exitscope();
  // check type of expression conforms to type of attribute, allow no_type since that means there was no init
  if (init->get_type() != No_type && !ctab->is_parent(type_decl, init->get_type(), class_name)) {
    ctab->semant_error(filename, this) << "Error: Init expression type " << init->get_type() << " does not conform to declared attr type "
				       << type_decl << endl;
  }
}

void method_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  // environment where SELF_TYPE maps to the class name
  Symbol return_type_local;
  return_type == SELF_TYPE ? return_type_local = class_name : return_type_local = return_type;

  osymtab->enterscope();
  osymtab->addid(self, SELF_TYPE);

  // check each formal
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formals->nth(i)->semant(ctab, osymtab, class_name, filename);
  }
  // check return type
  if (!ctab->entry_exists(return_type_local)) {
    ctab->semant_error(filename, this) << "Error: Return type " << return_type_local << " is not defined" << endl;
  }
  // get type of expression
  expr->semant(ctab, osymtab, class_name, filename);
  // check expression type
  if (!ctab->is_parent(return_type, expr->get_type(), class_name)) {
    ctab->semant_error(filename, this) << "Error: Method expression type " << expr->get_type() << " does not conform to return type "
				       << return_type_local << endl;
  }
  osymtab->exitscope();
}

void formal_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  // check if we've already defined the name
  if (osymtab->probe(name) != NULL) {
    ctab->semant_error(filename, this) << "Error: Formal name " << name << " is already defined in the method signature" << endl;
  }
  else {
    // check if type exists -- if not, use object
    if (!ctab->entry_exists(type_decl)) {
      ctab->semant_error(filename, this) << "Error: Formal type " << type_decl << " is not defined" << endl;
      osymtab->addid(name, Object);
    }
    else {
      osymtab->addid(name, type_decl);
    }
  }
}

void branch_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  osymtab->enterscope();
  if (!ctab->entry_exists(type_decl)) {
    ctab->semant_error(filename, this) << "Error: Branch type declaration " << type_decl << " is undefined" << endl;
    osymtab->addid(name, Object);
  }
  else {
    osymtab->addid(name, type_decl);
  }
  expr->semant(ctab, osymtab, class_name, filename);
  type = expr->get_type();
  osymtab->exitscope();
  // LOC1
}

void assign_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  if (name == self) {
    ctab->semant_error(filename, this) << "Error: Cannot redefine self" << endl;
  }

  Symbol id_type = osymtab->lookup(name);

  // check if the id is in scope
  if (id_type == NULL) {
    ctab->semant_error(filename, this) << "Error: Variable " << name << " not defined in the current scope" << endl;
    id_type = Object;
  }

  // eval the type of the expression
  expr->semant(ctab, osymtab, class_name, filename);

  // check expression type
  if (!ctab->is_parent(id_type, expr->get_type())) {
    ctab->semant_error(filename, this) << "Error: Assignment expression type " << expr->get_type() << " does not conform to ID type "
				       << id_type << endl;
    type = Object;
  }
  else {
    type = expr->get_type();
  }
  
}

void static_dispatch_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  // expr, name, actual
  int num_args = 0;
  expr->semant(ctab, osymtab, class_name, filename);
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->semant(ctab, osymtab, class_name, filename);
    num_args++;
  }
  // check that static dispatch type exists
  if (!ctab->entry_exists(type_name)) {
    ctab->semant_error(filename, this) << "Error: Static dispatch type " << type_name << " is not defined" << endl;
  }
  // check that expression conforms to dispatch type
  if (!ctab->is_parent(type_name, expr->get_type(), class_name)) {
    ctab->semant_error(filename, this) << "Error: Expression type " << expr->get_type() 
				       << " must conform to static dispatch type " << type_name << endl;
  }

  Signature* method_signature = ctab->get_entry(type_name)->get_msymtab()->lookup(name);
 
  // check that method exists for this type
  if (method_signature == NULL) {
    ctab->semant_error(filename, this) << "Error: Method " << name << " is not defined for class " << expr->get_type() << endl;
    type = Object;
  }
  else {
    Symbol return_type = method_signature->hd();
    List<Entry>* arg_types = method_signature->tl();
    // check that number of arguments is correct;
    if (num_args != list_length(arg_types)) {
      ctab->semant_error(filename, this) << "Error: Method dispatch for " << name << " has the incorrect number of arguments" << endl;
    }
    else {
      int arg_ind = 1;
      // check the type of each argument conforms to the signature
      for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
	if (!ctab->is_parent(arg_types->hd(), actual->nth(i)->get_type(), class_name)) {
	  ctab->semant_error(filename, this) << "Error: Arg " << arg_ind << " of method " << name << " with type " 
					     << actual->nth(i)->get_type() << " does not conform to type " << arg_types->hd() 
					     << " declared in signature" << endl;
	}
	arg_types = arg_types->tl();
	arg_ind++;
      }
    }
    // if return type is SELF_TYPE, then set to type of calling expression
    return_type == SELF_TYPE ? type = expr->get_type() : type = return_type;
  }
}

void dispatch_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  // expr, name, actual
  int num_args = 0;
  expr->semant(ctab, osymtab, class_name, filename);
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->semant(ctab, osymtab, class_name, filename);
    num_args++;
  }
  Signature* method_signature = ctab->get_entry(expr->get_type(), class_name)->get_msymtab()->lookup(name);
 
  // check that method exists for this type
  if (method_signature == NULL) {
    ctab->semant_error(filename, this) << "Error: Method " << name << " is not defined for class " << expr->get_type() << endl;
    type = Object;
  }
  else {
    Symbol return_type = method_signature->hd();
    List<Entry>* arg_types = method_signature->tl();
    // check that number of arguments is correct;
    if (num_args != list_length(arg_types)) {
      ctab->semant_error(filename, this) << "Error: Method dispatch for " << name << " has the incorrect number of arguments" << endl;
    }
    else {
      int arg_ind = 1;
      // check the type of each argument conforms to the signature
      for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
	if (!ctab->is_parent(arg_types->hd(), actual->nth(i)->get_type(), class_name)) {
	  ctab->semant_error(filename, this) << "Error: Arg " << arg_ind << " of method " << name << " with type " 
					     << actual->nth(i)->get_type() << " does not conform to type " << arg_types->hd() 
					     << " declared in signature" << endl;
	}
	arg_types = arg_types->tl();
	arg_ind++;
      }
    }
    // if return type is SELF_TYPE, then set to type of calling expression
    return_type == SELF_TYPE ? type = expr->get_type() : type = return_type;
  }
}

void cond_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  pred->semant(ctab, osymtab, class_name, filename);
  then_exp->semant(ctab, osymtab, class_name, filename);
  else_exp->semant(ctab, osymtab, class_name, filename);

  if (pred->get_type() != Bool) {
    ctab->semant_error(filename, this) << "Error: Type of conditional predicate is " << pred->get_type() << " but must be Bool" << endl;
  }
  type = ctab->get_common_parent(then_exp->get_type(), else_exp->get_type(), class_name);
}

void loop_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  pred->semant(ctab, osymtab, class_name, filename);
  body->semant(ctab, osymtab, class_name, filename);

  if (pred->get_type() != Bool) {
    ctab->semant_error(filename, this) << "Error: Type of loop predicate is " << pred->get_type() << " but must be Bool" << endl;
  }
  type = Object;
}

void typcase_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  expr->semant(ctab, osymtab, class_name, filename);

  // run semant on each branch and get the join of all branch types
  Symbol join_type = NULL;
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    cases->nth(i)->semant(ctab, osymtab, class_name, filename);
    join_type = ctab->get_common_parent(cases->nth(i)->get_type(), join_type);
  }

  
  // make sure types of branches are unique
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Symbol outer = cases->nth(i)->get_type_decl();
    for (int j = cases->next(i); cases->more(j); j = cases->next(j)) {
      Symbol inner = cases->nth(j)->get_type_decl();
      if (inner == outer) {
	ctab->semant_error(filename, this) << "Error: Repeated type " << inner << " in case branches" << endl;
      }
    }
    }

  // type of the whole expression is the join of types of branches
  type = join_type;
}

void block_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  Symbol last_expr_type;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->semant(ctab, osymtab, class_name, filename);
    last_expr_type = body->nth(i)->get_type();
  }
  type = last_expr_type;
}

void let_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  osymtab->enterscope();

  // check if type exists -- if not, use object
  if (!ctab->entry_exists(type_decl, class_name)) {
    ctab->semant_error(filename, this) << "Error: Let ID type " << type_decl << " is not defined" << endl;
    osymtab->addid(identifier, Object);
  }
  else if (identifier == self) {
    ctab->semant_error(filename, this) << "Error: Cannot use self as ID in Let statement" << endl;
  }
  else {
    osymtab->addid(identifier, type_decl);
  }
  
  // check that init conforms to ID type
  init->semant(ctab, osymtab, class_name, filename);
  if (init->get_type() != No_type && !ctab->is_parent(type_decl, init->get_type(), class_name)) {
    ctab->semant_error(filename, this) << "Error: Let initialization type " << init->get_type()
				       << " does not conform to declared type " << type_decl << endl;
  }
  // evaluate Let body, set type of let expression to type of body
  body->semant(ctab, osymtab, class_name, filename);
  type = body->get_type();
  osymtab->exitscope();
}

void plus_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);
  e2->semant(ctab, osymtab, class_name, filename);

  // both input expressions must be type Int
  if (e1->get_type() != Int || e2->get_type() != Int) {
    ctab->semant_error(filename, this) << "Error: Input types " << e1->get_type() << ", " << e2->get_type() 
				       << " to arithmetic operator must be Int, Int" << endl;
  }
  type = Int;
}

void sub_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);
  e2->semant(ctab, osymtab, class_name, filename);

  // both input expressions must be type Int
  if (e1->get_type() != Int || e2->get_type() != Int) {
    ctab->semant_error(filename, this) << "Error: Input types " << e1->get_type() << ", " << e2->get_type() 
				       << " to arithmetic operator must be Int, Int" << endl;
  }
  type = Int;
}

void mul_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);
  e2->semant(ctab, osymtab, class_name, filename);

  // both input expressions must be type Int
  if (e1->get_type() != Int || e2->get_type() != Int) {
    ctab->semant_error(filename, this) << "Error: Input types " << e1->get_type() << ", " << e2->get_type() 
				       << " to arithmetic operator must be Int, Int" << endl;
  }
  type = Int;
}

void divide_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);
  e2->semant(ctab, osymtab, class_name, filename);

  // both input expressions must be type Int
  if (e1->get_type() != Int || e2->get_type() != Int) {
    ctab->semant_error(filename, this) << "Error: Input types " << e1->get_type() << ", " << e2->get_type() 
				       << " to arithmetic operator must be Int, Int" << endl;
  }
  type = Int;
}

void neg_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);

  // input expressions must be type Int
  if (e1->get_type() != Int) {
    ctab->semant_error(filename, this) << "Error: Input type " << e1->get_type() 
				       << " to negations operator must be Int" << endl;
  }
  type = Int;
}

void lt_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);
  e2->semant(ctab, osymtab, class_name, filename);

  // both input expressions must be type Int
  if (e1->get_type() != Int || e2->get_type() != Int) {
    ctab->semant_error(filename, this) << "Error: Input types " << e1->get_type() << ", " << e2->get_type() 
				       << " to comparison operator must be Int, Int" << endl;
  }
  type = Bool;
}

void eq_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);
  e2->semant(ctab, osymtab, class_name, filename);

  if ((e1->get_type() == Int || e1->get_type() == Bool || e1->get_type() == Str ||
       e2->get_type() == Int || e2->get_type() == Bool || e2->get_type() == Str ) 
      && e1->get_type() != e2->get_type()) {
    ctab->semant_error(filename, this) << "Error: Attempting to compare " << e1->get_type() << " and " << e2->get_type()
				       << ". Can only compare primitives (Int, Bool, String) with same type" << endl;
  }
  type = Bool;
}

void leq_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);
  e2->semant(ctab, osymtab, class_name, filename);

  // both input expressions must be type Int
  if (e1->get_type() != Int || e2->get_type() != Int) {
    ctab->semant_error(filename, this) << "Error: Input types " << e1->get_type() << ", " << e2->get_type() 
				       << " to comparison operator must be Int, Int" << endl;
  }
  type = Bool;
}

void comp_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);

  // input expressions must be type Int
  if (e1->get_type() != Bool) {
    ctab->semant_error(filename, this) << "Error: Input type " << e1->get_type() 
				       << " to complement operator must be Bool" << endl;
  }
  type = Bool;
}

void int_const_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  type = Int;
}

void bool_const_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  type = Bool;
}

void string_const_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  type = Str;
}

void new__class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  Symbol type_name_local;
  type_name == SELF_TYPE ? type_name_local = class_name : type_name_local = type_name;

  // check type exists
  if (!ctab->entry_exists(type_name_local)) {
    ctab->semant_error(filename, this) << "Error: Type name of new expression " << type_name_local << " is not defined" << endl;
    type = Object;
  }
  else {
    type = type_name;
  }
}

void isvoid_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  e1->semant(ctab, osymtab, class_name, filename);
  type = Bool;
}

void no_expr_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  type = No_type;
}

void object_class::semant(ClassTable* ctab, SymbolTable<Symbol, Entry>* osymtab, Symbol class_name, Symbol filename) {
  Symbol object_type = osymtab->lookup(name);

  if (object_type == NULL) {
    ctab->semant_error(filename, this) << "Error: ID " << name << " not defined in the current scope" << endl;
    type = Object;
  }
  else {
    type = object_type;
  }
}

