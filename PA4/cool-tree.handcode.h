 * code() methods take it as an argument
 */
class Environment;
class Location;

typedef SymbolTable<Symbol,int> ClassMethodTable;
typedef SymbolTable<Symbol,ClassMethodTable> MethodTable;
typedef SymbolTable<Symbol,Location> ObjectTable;
typedef SymbolTable<Symbol,List<Entry> > ChildTable;
typedef SymbolTable<Symbol,int> TagTable;

// a pairing of register and offset from register to find an object
class Location
{
 private:
  char* reg;
  int offset;
 public:
  Location(char* r, int o) : reg(r), offset(o) {}
  char* get_reg() { return reg; }
  int get_offset() { return offset; }
};

class Environment {
 public:
  MethodTable* mlookup;
  ObjectTable* olookup;
  ChildTable* clookup;
  TagTable* tlookup;
  Symbol currclass; // the current class that we are emitting code in
  Symbol currfile;  // filename of the current class we are emitting code in
  int labelcounter; // so we never duplicate labels
  int stackcounter; // keep track of variables on the stack
 Environment(ChildTable* ctab, TagTable* ttab) 
                : mlookup(new MethodTable()), 
                  olookup(new ObjectTable()),
                  clookup(ctab),
                  tlookup(ttab),
		  labelcounter(0),
                  stackcounter(0) {}
  int get_method_offset(Symbol, Symbol);
  Location* get_object_location(Symbol); // in cgen.cc
  int pop_label() { return labelcounter++; }
  void push_stack() { stackcounter++; }
  void pop_stack() { stackcounter--; }
  void push_stack(int n) { for (int i = 0; i < n; i++) { push_stack(); } }
  void pop_stack(int n) { for (int i = 0; i < n; i++) { pop_stack(); } }
  int get_stack() { return stackcounter; }
  List<Entry>* get_children_names(Symbol cname) { return clookup->lookup(cname); }
  int get_classtag(Symbol cname) { return *tlookup->lookup(cname); }
};


#define Program_EXTRAS                          \
virtual void cgen(ostream&) = 0;		\
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS                          \
void cgen(ostream&);     			\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_name() = 0;  	\
virtual Symbol get_parent() = 0;    	\
virtual Symbol get_filename() = 0;      \
virtual Features get_features() = 0;    \
virtual void dump_with_types(ostream&,int) = 0; 


#define class__EXTRAS                                  \
Symbol get_name()   { return name; }		       \
Symbol get_parent() { return parent; }     	       \
Symbol get_filename() { return filename; }             \
Features get_features() { return features; }           \
void dump_with_types(ostream&,int);                    


#define Feature_EXTRAS                               \
virtual bool is_method() = 0;                        \
virtual Symbol get_name() = 0;                       \
virtual void dump_with_types(ostream&,int) = 0; 

#define method_EXTRAS                                \
bool is_method() { return true; }                    \
Expression get_expr() { return expr; }               \
int numargs() { return formals->len(); }             \
Formals get_formals() { return formals; }

#define attr_EXTRAS                                  \
bool is_method() { return false; }                   \
Symbol get_type() { return type_decl; }              \
Expression get_expr() { return init; }


#define Feature_SHARED_EXTRAS                        \
Symbol get_name() { return name; }                   \
void dump_with_types(ostream&,int);    


#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;    \
virtual Symbol get_name() = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);             \
Symbol get_name() { return name; }


#define Case_EXTRAS                              \
virtual void dump_with_types(ostream& ,int) = 0; \
virtual Symbol get_type() = 0;                   \
virtual Symbol get_name() = 0;                   \
virtual Expression get_expr() = 0;


#define branch_EXTRAS                           \
void dump_with_types(ostream& ,int);            \
Symbol get_type() { return type_decl; };	\
 Symbol get_name() { return name; }		\
 Expression get_expr() { return expr; };


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void code(ostream&, Environment* env) = 0;    	 \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }

#define Expression_SHARED_EXTRAS           \
void code(ostream&, Environment* env);			   \
void dump_with_types(ostream&,int); 


#endif

