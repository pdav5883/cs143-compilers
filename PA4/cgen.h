#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class MData;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_protobjs();
   void code_classtabs();
   void code_disptabs();
   void code_methods();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   List<CgenNode>* sort_classes();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int classtag;                              // The classtag of this class
   int numattrs;                              // Number of attrs in protObj
   int nummethods;                            // Number of methods in dispTab
   SymbolTable<Symbol,int>* attrtab;          // maps attr name to ind
   SymbolTable<int,Entry>* rattrtab;         // maps attr ind to name
   SymbolTable<Symbol,MData>* methodtab;      // maps method name to ind and implementing class
   SymbolTable<int,MData>* rmethodtab;       // maps ind to method name and implementing class


public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   List<Entry>* get_children_names();
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   int get_classtag() { return classtag; }
   int get_numattrs() { return numattrs; }
   int get_nummethods() { return nummethods; }
   int build_feature_tables(SymbolTable<Symbol,int>*,
			    SymbolTable<int,Entry>*,
			    SymbolTable<Symbol,MData>*,
			    SymbolTable<int,MData>*, int, int, int);
   int get_attr_offset(Symbol);
   Symbol get_attr_name(int);
   int get_method_offset(Symbol);
   MData* get_method_data(Symbol);
   MData* get_method_data(int);
   void code_init(Environment*, ostream&);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

// data about a method to be stored in method SymbolTables
class MData
{
 private:
  Symbol cname; // the class that defines the method
  Symbol mname; // the name of the method
  int ind;      // the index of the method in the dispatch table
 public:
  MData(Symbol c, Symbol m, int i) : cname(c), mname(m), ind(i) {}
  Symbol get_cname() { return cname; }
  Symbol get_mname() { return mname; }
  int get_ind() { return ind; }
};

void build_branchlookup(Symbol, SymbolTable<Symbol, int>*, Environment*);




