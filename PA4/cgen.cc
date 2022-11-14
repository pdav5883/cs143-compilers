
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";
  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Pop the top of the stack to a register and move stack pointer.
//
static void emit_pop(char *reg, ostream& str)
{
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,4,str);
}

//
// Create a new object, store in acc
//
static void emit_new_obj(Symbol type_name, ostream& str)
{
  // load protobj into acc
  emit_partial_load_address(ACC, str);
  emit_protobj_ref(type_name, str);
  str << endl;
  
  // jump to copy
  emit_jal("Object.copy", str);

  // jump to init
  str << JAL;
  emit_init_ref(type_name, str);
  str << endl;
} 

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;
      s << STRINGNAME << DISPTAB_SUFFIX << endl;                       // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 
      s << INTNAME << DISPTAB_SUFFIX << endl;             // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;
      s << BOOLNAME << DISPTAB_SUFFIX << endl;              // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // Global names for class tables, proto objects, and dispatch tables
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL << CLASSOBJTAB << endl;

  for (List<CgenNode>* l = nds; l; l = l->tl()) {
    str << GLOBAL; emit_protobj_ref(l->hd()->get_name(), str); str << endl;
    str << GLOBAL; emit_disptable_ref(l->hd()->get_name(), str); str << endl;
  }
  
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << "\n\n" << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl;

  for (List<CgenNode>* l = nds; l; l = l->tl()) {
    str << GLOBAL; emit_init_ref(l->hd()->get_name(), str); str << endl;
  }

  str << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

//**********************************************************
//
// Emit code to build protObjs, class_NameTab, class_ObjTab,
// and dispatch tables
//
//**********************************************************

// define protObj for each class in the inheritance tree
void CgenClassTable::code_protobjs()
{
  for (List<CgenNode>* l = nds; l; l = l->tl()) {
    //str << "\n" << GLOBAL; emit_protobj_ref(l->hd()->get_name(), str); str << endl; // set global label
    str << WORD << -1 << endl;                                       // garbage collector tag
    emit_protobj_ref(l->hd()->get_name(), str); str << LABEL;        // define label
    str << WORD << l->hd()->get_classtag() << endl;                  // class tag
    str << WORD << DEFAULT_OBJFIELDS + l->hd()->get_numattrs() << endl;              // num words in obj
    str << WORD; emit_disptable_ref(l->hd()->get_name(), str); str << endl;            // dispatch table label

    // need a special case for String to set to ""
    if (l->hd()->get_name() == Str) {
      str << WORD; inttable.add_int(0)->code_ref(str); str << endl;   // string length is zero
      emit_string_constant(str, "");
      str << ALIGN;
    }
    else {
      for (int i = 0; i < l->hd()->get_numattrs(); i++)                // 0 word for each attr
	str << WORD << EMPTYSLOT << endl;                                      
    }
  }
}

// define class_nameTab, which has pointer to string object with class name at class tag ind
// define class_objTab, which has pointer to protObj then pointer to dispTab at class tag ind
void CgenClassTable::code_classtabs()
{
  // create a list of nodes sorted by classtag with classtag=0 at head
  List<CgenNode>* nds_sorted = sort_classes();

  // emit class_nameTab
  str << CLASSNAMETAB << LABEL;
  for(List<CgenNode>* l = nds_sorted; l; l = l->tl()) {
    str << WORD;
    stringtable.add_string(l->hd()->get_name()->get_string())->code_ref(str);
    str << endl;
  }

  // emit class_objTab
  str << CLASSOBJTAB << LABEL;
  for(List<CgenNode>* l = nds_sorted; l; l = l->tl()) {
    str << WORD; emit_protobj_ref(l->hd()->get_name(), str);   str << endl;
    str << WORD; emit_init_ref(l->hd()->get_name(), str); str << endl;
  }
}

void CgenClassTable::code_disptabs()
{
  // loop through all classes
  for (List<CgenNode>* l = nds; l; l = l->tl()) {

    // emit class_dispTab label
    emit_disptable_ref(l->hd()->get_name(), str); str << LABEL;
    // emit entry point label for each method
    for (int mind = 0; mind < l->hd()->get_nummethods(); mind++) {
      MData* mdat = l->hd()->get_method_data(mind);
      str << WORD << mdat->get_cname() << METHOD_SEP << mdat->get_mname() << endl;
    }
  }
}

// init routines for a given class
void CgenNode::code_init(Environment* env, ostream& str)
{
  emit_init_ref(get_name(), str); str << LABEL;

  // if this is a basic class we don't need to do anything but return
  if (basic()) {
    emit_return(str);
  }
  else {
    emit_push(RA, str);   // push return address
    emit_push(SELF, str); // push old self
    env->push_stack(2);
    emit_move(SELF, ACC, str); // set current self
    
    // create all attributes in current class before inits, use index in attrtab as offset
    str << "\t# start attr create" << endl;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      Feature feat = features->nth(i);
      
      // only do something for attributes
      if (!feat->is_method()) {
	attr_class* attr_feat = dynamic_cast<attr_class*>(feat);
	
	
	if (attr_feat->get_type() == Str)
	  { emit_load_string(ACC, stringtable.add_string(""), str); }
	else if (attr_feat->get_type() == Int)
	  { emit_load_int(ACC, inttable.add_int(0), str); }
	else if (attr_feat->get_type() == Bool)
	  { emit_load_bool(ACC, falsebool, str); }
	else
	  { emit_load_imm(ACC, 0, str); }
	
	// put pointer at correct attribute location in object
	emit_store(ACC, get_attr_offset(attr_feat->get_name()), SELF, str);
      }
    }
    str << "\t# end attr create" << endl;

    // jump to init of parent class to initialize attributes of ancestors
    // this needs to come after the creation of this class's attribs, since those attribs could get
    // called as part of dynamic dispatch init of super class attrs. SUPER TRICKY!
    emit_move(ACC, SELF, str);
    str << JAL; emit_init_ref(get_parentnd()->get_name(), str); str << endl;

    // go through all attributes again and code init expressions
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      Feature feat = features->nth(i);
      
      // only do something for attributes
      if (!feat->is_method()) {
	attr_class* attr_feat = dynamic_cast<attr_class*>(feat);
	
	// check for init expr
	if (attr_feat->get_expr()->get_type() != NULL) {
	  str << "\t# start init expression" << endl;
	  attr_feat->get_expr()->code(str, env); // init result stored in acc
	  emit_store(ACC, get_attr_offset(attr_feat->get_name()), SELF, str); // store pointer in object
	  str << "\t# end init expression" << endl;
	}
      }
    }
    
    emit_move(ACC, SELF, str); // set ACC to current self
    emit_pop(SELF, str);  // pop old self
    emit_pop(RA, str);    // pop return address
    env->pop_stack(2);
    emit_return(str);
  }
}

// emit code for each class method (callee side)
void CgenClassTable::code_methods()
{
  // build lookup tables for class children and tags to pass to environment
  ChildTable* clookup = new ChildTable();
  TagTable* tlookup = new TagTable();
  clookup->enterscope();
  tlookup->enterscope();
  
  for (List<CgenNode>* l = nds; l; l = l->tl()) {
    clookup->addid(l->hd()->get_name(), l->hd()->get_children_names());
    tlookup->addid(l->hd()->get_name(), new int(l->hd()->get_classtag()));
  }

  // create the environment by building the method lookup for all classes
  Environment* env = new Environment(clookup, tlookup);
  env->mlookup->enterscope();
  
  // loop through all classes to build method tables
  for (List<CgenNode>* l = nds; l; l = l->tl()) {

    ClassMethodTable* cmlookup = new ClassMethodTable();
    cmlookup->enterscope();

    // loop through all methods
    for (int i = 0; i < l->hd()->get_nummethods(); i++) {
      MData* mdat = l->hd()->get_method_data(i);
      cmlookup->addid(mdat->get_mname(), new int(i)); // add method to inner table
    }
    env->mlookup->addid(l->hd()->get_name(), cmlookup); // add inner table to outer table
  }

  // loop through all non-basic classes to generate code
  for (List<CgenNode>* l = nds; l; l = l->tl()) {
    CgenNode* nd = l->hd();

    // set current class of environment
    env->currclass = nd->get_name();
    env->currfile = nd->get_filename();
    
    // start object environment for this class by adding attributes -- need to do this before init
    // so that any expressions in init can reference attributes of the class
    env->olookup->enterscope();

    // this is kind of silly, we're just copying attrtab slowly
    // note that attributes are stored in memory space AFTER object pointer since they're on heap
    for (int i = 0; i < l->hd()->get_numattrs(); i++) {
      env->olookup->addid(nd->get_attr_name(i), new Location(SELF, i + DEFAULT_OBJFIELDS));
    }

    // emit init method for this class
    nd->code_init(env, str);

    if (!nd->basic()) {
      // emit methods defined in current class
      for (int i = nd->features->first(); nd->features->more(i); i = nd->features->next(i)) {
	Feature feat = nd->features->nth(i);

	// only do something for methods
	if (feat->is_method()) {
	  method_class* method_feat = dynamic_cast<method_class*>(feat);
	  int n = method_feat->numargs();

	  // method creates another scope in environment object lookup containing args relative to frame pointer
	  env->olookup->enterscope();
	  Formals formals = method_feat->get_formals();
	  int argoffset = 0; // where to find the argument relative to frame pointer
	  // note that arguments are stored in memory space BEFORE frame pointer since they're on stack, so offset decrements
	  for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
	    env->olookup->addid(formals->nth(j)->get_name(), new Location(FP, argoffset));
	    argoffset--;
	  } 

	  // entrypoint label
	  emit_method_ref(nd->get_name(), method_feat->get_name(), str);
	  str << LABEL;

	  // push callee-saved registers onto stack
	  // note: don't need to track env stackcounter here since we can never be inside let/case at this point
	  emit_push(FP, str);   // the previous frame pointer
	  emit_push(SELF, str); // the previous self
	  emit_push(RA, str);   // return
	  emit_move(SELF, ACC, str); // the current self
	  emit_addiu(FP, SP, 12 + 4 * n, str); // frame pointer at start of current frame

	  // emit code for the method body, result in ACC
	  str << "\t# start of expression body" << endl;
	  method_feat->get_expr()->code(str, env);
	  str << "\t# end of expression body" << endl;

	  // pop callee-saved registers off the stack
	  emit_pop(RA, str);
	  emit_pop(SELF, str);
	  emit_pop(FP, str);

	  // pop arguments
	  emit_addiu(SP, SP, 4 * n, str);
	  
	  // return to caller
	  emit_return(str);

	  // back out of method scope
	  env->olookup->exitscope();
	}
      }
    }
    env->olookup->exitscope(); // remove attributes once we are done with this class
  }
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//

  code_protobjs();
  code_classtabs();
  code_disptabs();

  // build class_nameTab and class_objTab
  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  code_methods(); // includes init

}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   // build attrTab and methodTab to encode offsets for each class, assign class tags
   root()->build_feature_tables(new SymbolTable<Symbol,int>(),
				new SymbolTable<int,Entry>(),
				new SymbolTable<Symbol,MData>(),
				new SymbolTable<int,MData>(),
				0, 0, 0);

   stringclasstag = lookup(Str)->get_classtag();
   intclasstag = lookup(Int)->get_classtag();
   boolclasstag = lookup(Bool)->get_classtag();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

// create list of class nodes sorted by classtag, with classtag=0 at head
List<CgenNode>* CgenClassTable::sort_classes()
{
  List<CgenNode>* lrev = NULL;
  int ind = 0;
  bool found = true;

  // sort by classtag, but classtag=0 will be at tail
  while (found) {
    found = false;
    for (List<CgenNode>* l = nds; l; l = l->tl()) {
      if (l->hd()->get_classtag() == ind) {
	lrev = new List<CgenNode>(l->hd(), lrev);
	ind++;
	found = true;
	break;
      }
    }
  }

  // reverse to get classtag=0 at head
  List<CgenNode>* lsort = NULL;
  while (lrev != NULL) {
    lsort = new List<CgenNode>(lrev->hd(), lsort);
    lrev = lrev->tl();
  }
  return lsort;
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

// set attrlist, methodlist, and classtag then call on all children
// return the last used classtag
int CgenNode::build_feature_tables(SymbolTable<Symbol,int>* attrtab_parent,
				   SymbolTable<int,Entry>* rattrtab_parent,
				   SymbolTable<Symbol,MData>* methodtab_parent,
				   SymbolTable<int,MData>* rmethodtab_parent,
				   int classtag_in, int attrind, int methodind)
{
  attrtab = attrtab_parent->clone();
  rattrtab = rattrtab_parent->clone();
  methodtab = methodtab_parent->clone();
  rmethodtab = rmethodtab_parent->clone();
  classtag = classtag_in;

  // go through features and pull out attributes, methods
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    Feature feat = features->nth(i);

    if (feat->is_method()) {
      // method is not defined in parent class, increment methodind
      if (methodtab->lookup(feat->get_name()) == NULL) {
	MData* mdatanew = new MData(get_name(), feat->get_name(), methodind);
	methodtab->addid(feat->get_name(), mdatanew);
	rmethodtab->addid(methodind, mdatanew);
	methodind++;
      }
      // method defined in parent class, use that methodind
      else {
  	MData* mdatanew = new MData(get_name(), feat->get_name(), methodtab->lookup(feat->get_name())->get_ind());
	methodtab->addid(feat->get_name(), mdatanew);
	rmethodtab->addid(methodtab->lookup(feat->get_name())->get_ind(), mdatanew);
      }
    }
    else {
      attrtab->addid(feat->get_name(), new int(attrind));
      rattrtab->addid(attrind, feat->get_name());
      attrind++;
    }
  }
 
  // attrind and methodind are zero-indexed
  numattrs = attrind;
  nummethods = methodind;

  // call recursively on all children, keeping track of classtag
  int classtag_out = classtag_in + 1;
  for(List<CgenNode> *l = children; l; l = l->tl()) {
    classtag_out = l->hd()->build_feature_tables(attrtab, rattrtab, methodtab, rmethodtab, classtag_out, attrind, methodind);
  }
  return classtag_out;
}

// the number of words an attribute is offset from the object address
int CgenNode::get_attr_offset(Symbol aname) {
  return *attrtab->lookup(aname) + DEFAULT_OBJFIELDS;
}

// the attribute located at index
Symbol CgenNode::get_attr_name(int ind) {
  return rattrtab->lookup(ind);
}

// the number of words a method entry point is offset in the dispatch table
int CgenNode::get_method_offset(Symbol mname) {
  return get_method_data(mname)->get_ind();
}

MData* CgenNode::get_method_data(Symbol mname) {
  return methodtab->lookup(mname);
}

MData* CgenNode::get_method_data(int mind) {
  return rmethodtab->lookup(mind);
}

List<Entry>* CgenNode::get_children_names() {
  List<Entry>* names = NULL;

  for (List<CgenNode>* l = get_children(); l; l = l->tl()) {
    names = new List<Entry>(l->hd()->get_name(), names);
  }

  return names;
}

int Environment::get_method_offset(Symbol cname, Symbol mname) {
    if (cname == SELF_TYPE)
      cname = currclass;
    
    return *  mlookup->lookup(cname)->lookup(mname);
  }

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: assign" << endl;

  // generate code for the RHS expresssion, with result in acc
  expr->code(s, env);

  // store result at location of referenced object
  Location* loc = env->get_object_location(name);
  emit_store(ACC, loc->get_offset(), loc->get_reg(), s);

  s << "\t# end expr: assign" << endl;
}

void static_dispatch_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: static dispatch of " << name << endl;
  // arguments e1 to en are evaluated and put on stack
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, env);
    emit_push(ACC, s);
    env->push_stack();
  }

  // evaluate the e0 expr, which ends up in acc
  expr->code(s, env);

  // load addr of dispatch table for static type of dispatch
  emit_partial_load_address(T1, s); emit_disptable_ref(type_name, s); s << endl;

  // lookup the offset of the method being called using the static type of dispatch
  int offset = env->get_method_offset(type_name, name);

  // grab the address at T1 (start of disptab) plus offset 
  emit_load(T1, offset, T1, s);

  // jump to the address that T1 is pointing two
  emit_jalr(T1, s);

  // after return, adjust env for args that were popped off on callee side
  // note we can't actually pop off args here, since trap.handler already pops for core methods
  env->pop_stack(actual->len());

  s << "\t# end expr: static dispatch of " << name << endl;
}

void dispatch_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: dispatch of " << name << endl;
  // arguments e1 to en are evaluated and put on stack
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, env);
    emit_push(ACC, s);
    env->push_stack();
  }

  // evaluate the e0 expr, which ends up in acc
  expr->code(s, env);

  // runtime check of whether we are dispatching on void
  // if object is void (zero), put filename in ACC and line num in T1 and call _dispatch_abort
  int label_void = env->pop_label();
  int label_end = env->pop_label();

  s << "# start dispatch void check" << endl;
  emit_beqz(ACC, label_void, s);
  emit_branch(label_end, s);
  emit_label_def(label_void, s);
  emit_load_imm(T1, get_line_number(), s); // line number goes into T1
  emit_load_string(ACC, stringtable.lookup_string(env->currfile->get_string()), s); // first the address filename String into ACC
  emit_jal("_dispatch_abort", s);
  emit_label_def(label_end, s);
  s << "# end dispatch void check" << endl;

  // load addr of dispatch table for a0 type
  emit_load(T1, 2, ACC, s);

  // lookup the offset of the method being called using the static type of e0 (offset is same as dynamic type)
  int offset = env->get_method_offset(expr->get_type(), name);

  // grab the address at T1 (start of disptab) plus offset 
  emit_load(T1, offset, T1, s);

  // jump to the address that T1 is pointing two
  emit_jalr(T1, s);

  // after return, adjust env for args that were popped off on callee side
  // note we can't actually pop off args here, since trap.handler already pops for core methods
  env->pop_stack(actual->len());
  
  s << "\t# end expr: dispatch of " << name << endl;
}

void cond_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: conditional" << endl;
  // generate code for predicate
  pred->code(s, env);

  // grab the boolean value
  emit_fetch_int(ACC, ACC, s);

  // create labels for false and end
  int label_false = env->pop_label();
  int label_end = env->pop_label();

  // branch on false
  emit_beqz(ACC, label_false, s);

  // then branch, jump to end
  then_exp->code(s, env);
  emit_branch(label_end, s);

  // else branch
  emit_label_def(label_false, s);
  else_exp->code(s, env);

  // end
  emit_label_def(label_end, s);

  s << "\t# end expr: conditional" << endl;
}

void loop_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: loop" << endl;

  int label_pred = env->pop_label();
  int label_end = env->pop_label();

  // label predicate, eval
  emit_label_def(label_pred, s);

  // eval predicate, jump past loop if false
  pred->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  emit_beqz(ACC, label_end, s);

  // eval body, go back to predicate
  body->code(s, env);
  emit_branch(label_pred, s);

  // end, set ACC to void
  emit_label_def(label_end, s);
  emit_load_imm(ACC, 0, s);
  
  s << "\t# end expr: loop" << endl;
}

void typcase_class::code(ostream &s, Environment* env) {
  // blookup: class name --> branch label integer
  // tlookup (in env): class name --> class tag integer

  // build branch lookup for this case block
  SymbolTable<Symbol, int>* blookup = new SymbolTable<Symbol, int>();
  blookup->enterscope();

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    blookup->addid(cases->nth(i)->get_type(), new int(env->pop_label()));
  }

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    build_branchlookup(cases->nth(i)->get_type(), blookup, env);
  }

  s << "\t# start expr: case" << endl;
  
  // evaluate the expression
  expr->code(s, env);

  // if void trigger runtime error
  s << "\t# check expr void" << endl;
  int label_void = env->pop_label();
  emit_beqz(ACC, label_void, s);

  // push on stack, and retain class tag
  emit_push(ACC, s);
  emit_load(ACC, 0, ACC, s);

  // emit check for each of the classes in branch lookup
  for (List<SymtabEntry<Symbol,int> >* l = blookup->flatten(); l; l = l->tl()) {
    Symbol cname = l->hd()->get_id();
    int label = *l->hd()->get_info();
    int tag = env->get_classtag(cname);

    s << "\t# check class " << cname << endl;
    emit_load_imm(T1, tag, s);
    emit_beq(ACC, T1, label, s);
  }

  // runtime error no branch -- class name in ACC
  s << "\t# error: no matching case" << endl;
  emit_pop(ACC, s);
  emit_jal("_case_abort", s);
  
  // runtime error void type -- filename in ACC and line number in T1
  emit_label_def(label_void, s);
  s << "\t# error: void expression" << endl;
  emit_load_imm(T1, get_line_number(), s);
  emit_load_string(ACC, stringtable.lookup_string(env->currfile->get_string()), s);
  emit_jal("_case_abort2", s);  

  // emit code for each case branch
  int label_end = env->pop_label();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    env->olookup->enterscope();
    int label_case = *blookup->lookup(cases->nth(i)->get_type());
    
    emit_label_def(label_case, s);
    s << "\t# branch " << cases->nth(i)->get_type() << endl;

    // bind expression to id of case
    emit_pop(ACC, s);
    env->olookup->addid(cases->nth(i)->get_name(), new Location(SP, env->get_stack()));
    emit_push(ACC, s);
    env->push_stack();

    // emit code for case, clear the stack, then branch to end of expression
    cases->nth(i)->get_expr()->code(s, env);
    emit_pop(T1, s);
    env->pop_stack();
    emit_branch(label_end, s);

    env->olookup->exitscope();
  }
  
  // emit end
  emit_label_def(label_end, s);

  s << "\t# end expr: case" << endl;
}

void block_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: block" << endl;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s, env);
  }
  s << "\t# end expr: block" << endl;
}

void let_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: let" << endl;
  env->olookup->enterscope();

  // create the default object if no init
  if (init->get_type() == NULL) {
    if (type_decl == Str)
      { emit_load_string(ACC, stringtable.add_string(""), s); }
    else if (type_decl == Int)
      { emit_load_int(ACC, inttable.add_int(0), s); }
    else if (type_decl == Bool)
      { emit_load_bool(ACC, falsebool, s); }
    else
      { emit_load_imm(ACC, 0, s); }
  }

  // otherwise run init
  else {
    init->code(s, env);
  }

  // put object on the stack and point environment to it
  env->olookup->addid(identifier, new Location(SP, env->get_stack()));
  emit_push(ACC, s);
  env->push_stack();

  // emit the body, result in ACC
  body->code(s, env);

  // clean up
  emit_pop(T1, s);
  env->pop_stack();
  env->olookup->exitscope();
  
  s << "\t# end expr: let" << endl;
}

void plus_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: plus" << endl;

  // eval first expression, grab value, push on stack
  e1->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  emit_push(ACC, s);
  env->push_stack();
  
  // eval second expression, grab value
  e2->code(s, env);
  emit_fetch_int(ACC, ACC, s);

  // add first value to second value, push on stack
  emit_pop(T1, s);
  env->pop_stack();
  emit_add(ACC, ACC, T1, s);
  emit_push(ACC, s);

  // create a new int object
  emit_new_obj(Int, s);

  // store calculated value in new object, which is in ACC
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
  
  s << "\t# end expr: plus" << endl;
}

void sub_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: sub" << endl;

  // eval first expression, grab value, push on stack
  e1->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  emit_push(ACC, s);
  env->push_stack();
  
  // eval second expression, grab value
  e2->code(s, env);
  emit_fetch_int(ACC, ACC, s);

  // add first value to second value, push on stack
  emit_pop(T1, s);
  env->pop_stack();
  emit_sub(ACC, T1, ACC, s);
  emit_push(ACC, s);

  // create a new int object
  emit_new_obj(Int, s);

  // store calculated value in new object, which is in ACC
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
  
  s << "\t# end expr: sub" << endl;
}

void mul_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: multiply" << endl;

  // eval first expression, grab value, push on stack
  e1->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  emit_push(ACC, s);
  env->push_stack();
  
  // eval second expression, grab value
  e2->code(s, env);
  emit_fetch_int(ACC, ACC, s);

  // add first value to second value, push on stack
  emit_pop(T1, s);
  env->pop_stack();
  emit_mul(ACC, ACC, T1, s);
  emit_push(ACC, s);

  // create a new int object
  emit_new_obj(Int, s);

  // store calculated value in new object, which is in ACC
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
  
  s << "\t# end expr: multiply" << endl;
}

void divide_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: divide" << endl;

  // eval first expression, grab value, push on stack
  e1->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  emit_push(ACC, s);
  env->push_stack();
  
  // eval second expression, grab value
  e2->code(s, env);
  emit_fetch_int(ACC, ACC, s);

  // add first value to second value, push on stack
  emit_pop(T1, s);
  env->pop_stack();
  emit_div(ACC, T1, ACC, s);
  emit_push(ACC, s);

  // create a new int object
  emit_new_obj(Int, s);

  // store calculated value in new object, which is in ACC
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
  
  s << "\t# end expr: divide" << endl;
}

void neg_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: negative" << endl;

  // eval expression, grab value, negate, push on stack
  e1->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  emit_neg(ACC, ACC, s);
  emit_push(ACC, s);

  // create a new int object
  emit_new_obj(Int, s);

  // store calculated value in new object, which is in ACC
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
  
  s << "\t# end expr: negative" << endl;
}

void lt_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: less than" << endl;

  // eval first expression, grab value, push on stack
  e1->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  emit_push(ACC, s);
  env->push_stack();
  
  // eval second expression, grab value
  e2->code(s, env);
  emit_fetch_int(ACC, ACC, s);

  // branch logic
  int label_set_true = env->pop_label();
  int label_end = env->pop_label();

  emit_pop(T1, s);
  env->pop_stack();
  emit_blt(T1, ACC, label_set_true, s);
  emit_load_imm(ACC, 0, s);
  emit_branch(label_end, s);
  emit_label_def(label_set_true, s);
  emit_load_imm(ACC, 1, s);
  emit_label_def(label_end, s);

  // create new bool and load value into it
  emit_push(ACC, s);
  emit_new_obj(Bool, s);
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);  

  s << "\t# end expr: less than" << endl;
}

void eq_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: equals" << endl;
  
  // generate code for expressions
  e1->code(s, env);
  emit_push(ACC, s);
  env->push_stack();
  e2->code(s, env);

  // if Int/Str/Bool then use equality_test routine in runtime, otherwise test for pointer equality
  if (e1->get_type() == Int || e1->get_type() == Str || e1->get_type() == Bool) {
    // save first expression in T1 and second in T2
    emit_pop(T1, s);
    env->pop_stack();
    emit_move(T2, ACC, s);

    // true goes in ACC, false in A1
    emit_load_imm(ACC, 1, s);
    emit_load_imm(A1, 0, s);

    // run test
    emit_jal("equality_test", s);

    // create boolean with result
    emit_push(ACC, s);
    emit_new_obj(Bool, s);
    emit_pop(T1, s);
    emit_store_int(T1, ACC, s);
  }
  
  // pointers equal?
  else {
    // branch logic
    int label_set_true = env->pop_label();
    int label_end = env->pop_label();

    emit_pop(T1, s);
    env->pop_stack();
    emit_beq(T1, ACC, label_set_true, s);
    emit_load_imm(ACC, 0, s);
    emit_branch(label_end, s);
    emit_label_def(label_set_true, s);
    emit_load_imm(ACC, 1, s);
    emit_label_def(label_end, s);
    
    // create new bool and load value into it
    emit_push(ACC, s);
    emit_new_obj(Bool, s);
    emit_pop(T1, s);
    emit_store_int(T1, ACC, s);
  } 

  s << "\t# end expr: equals" << endl;
}

void leq_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: less than equal" << endl;

  // eval first expression, grab value, push on stack
  e1->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  emit_push(ACC, s);
  env->push_stack();
  
  // eval second expression, grab value
  e2->code(s, env);
  emit_fetch_int(ACC, ACC, s);

  // branch logic
  int label_set_true = env->pop_label();
  int label_end = env->pop_label();

  emit_pop(T1, s);
  env->pop_stack();
  emit_bleq(T1, ACC, label_set_true, s);
  emit_load_imm(ACC, 0, s);
  emit_branch(label_end, s);
  emit_label_def(label_set_true, s);
  emit_load_imm(ACC, 1, s);
  emit_label_def(label_end, s);

  // create new bool and load value into it
  emit_push(ACC, s);
  emit_new_obj(Bool, s);
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);  

  s << "\t# end expr: less than equal" << endl;
}

void comp_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: complement" << endl;

  // eval expression, grab value, push on stack
  e1->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  
  int label_set_true = env->pop_label();
  int label_end = env->pop_label();

  // branch if false
  emit_beqz(ACC, label_set_true, s);
  
  // if true set false
  emit_load_imm(ACC, 0, s);
  emit_branch(label_end, s);

  // if false set true
  emit_label_def(label_set_true, s);
  emit_load_imm(ACC, 1, s);

  // end of branch
  emit_label_def(label_end, s);

  // push value on stack, create new bool, set new bool in ACC
  emit_push(ACC, s);
  emit_new_obj(Bool, s);
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
  
  s << "\t# end expr: complement" << endl;
}

void int_const_class::code(ostream& s, Environment* env)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  s << "\t# expr: int constant" << endl;
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, Environment* env)
{
  s << "\t# expr: string constant" << endl;
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, Environment* env)
{
  s << "\t# expr: bool constant" << endl;
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, Environment* env) {
  s << "\t# start expr: new" << endl;
  
  // use dynamic type of self
  if (type_name == SELF_TYPE) {
    // get class tag, convert to byte offset of protObj in classobjtab
    emit_load(ACC, 0, SELF, s); // class tag is zero offset from object pointer
    emit_load_imm(T1, 8, s);    // 2 words increment for each class tag increment, which is 8 bytes
    emit_mul(ACC, ACC, T1, s);

    // get class object table, increment by value in acc to get address of protobj, load into acc
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_addu(ACC, ACC, T1, s);
    emit_push(ACC, s);         // pointer to address of protobj
    emit_load(ACC, 0, ACC, s); // address of protobj

    // copy the protobj
    emit_jal("Object.copy", s);

    // now get the location of init function, which is one word further than protobj, then jump
    emit_pop(T1, s);           
    //emit_addiu(T1, T1, 4, s);  // pointer to address of init
    emit_load(T1, 1, T1, s);   // address of init
    emit_jalr(T1, s);
  }
  // use static type specific in expression
  else {
    emit_new_obj(type_name, s);
  }
  s << "\t# end expr: new" << endl;
}

void isvoid_class::code(ostream &s, Environment* env) {
  s << "\t# start expr: isvoid" << endl;

  e1->code(s, env);

  // if acc is zero, return true bool, otherwise false bool
  int label_set_true = env->pop_label();
  int label_end = env->pop_label();

  emit_beqz(ACC, label_set_true, s);
  emit_load_imm(ACC, 0, s);
  emit_branch(label_end, s);
  emit_label_def(label_set_true, s);
  emit_load_imm(ACC, 1, s);
  emit_label_def(label_end, s);

  // create a new bool and load value into it
  emit_push(ACC, s);
  emit_new_obj(Bool, s);
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);  

  s << "\t# end expr: isvoid" << endl;
}

// loads the default object into ACC depending on type
void no_expr_class::code(ostream &s, Environment* env) {
  s << "\t# expr: no expression" << endl;
}

void object_class::code(ostream &s, Environment* env) {
  if (name == self) {
    s << "\t# expr: object self" << endl;
    emit_move(ACC, SELF, s);
  }
  else {
    s << "\t# expr: object " << name << endl;
    Location* loc = env->get_object_location(name);
    emit_load(ACC, loc->get_offset(), loc->get_reg(), s);
  }
}

Location* Environment::get_object_location(Symbol oname) {
  Location* loc = olookup->lookup(oname);

  if (strcmp(loc->get_reg(), SP) != 0)
    return loc;

  return new Location(loc->get_reg(), get_stack() - loc->get_offset());
}

// recursive method to build branch mapping for case block
void build_branchlookup(Symbol parent, SymbolTable<Symbol, int>* blookup, Environment* env) {

  for (List<Entry>* l = env->get_children_names(parent); l; l = l->tl()) {
    Symbol child = l->hd();

    // if child class isn't in branch lookup, and child and its children
    if (blookup->lookup(child) == NULL) {
      blookup->addid(child, blookup->lookup(parent));
      build_branchlookup(child, blookup, env);
    }
  }
}

