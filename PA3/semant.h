#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

//class ClassTable; // this def moved to cool-tree.handcode.h since semant() needs it
class ClassTableEntry;
typedef ClassTable *ClassTableP;
typedef ClassTableEntry *ClassTableEntryP;
typedef List<Entry> Signature, *SignatureP;

class ClassTableEntry {
private:
  Symbol name;
  Class_ node;
  SymbolTable<Symbol, Entry> *osymtab;
  SymbolTable<Symbol, Signature> *msymtab;

public:
  ClassTableEntry(Class_);
  Symbol get_name() { return name; }
  Class_ get_node() { return node; }
  SymbolTable<Symbol, Entry>* get_osymtab() { return osymtab; }
  SymbolTable<Symbol, Signature>* get_msymtab() { return msymtab; }
  Symbol get_parent_name() { return node->get_parent(); }
  void add_attr(Feature);
  void add_method(Feature);
};

  
class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  List<ClassTableEntry> *tbl;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  ClassTableEntry* get_entry(Symbol);
  ClassTableEntry* get_entry(Symbol, Symbol);
  ClassTableEntry* get_parent_entry(Symbol);
  Symbol get_parent_name(Symbol);
  void add_entry(Class_);
  bool entry_exists(Symbol);
  bool entry_exists(Symbol, Symbol);
  bool is_parent(Symbol, Symbol);
  bool is_parent(Symbol, Symbol, Symbol);
  Symbol get_common_parent(Symbol, Symbol);
  Symbol get_common_parent(Symbol, Symbol, Symbol);
  void build_symtabs(Symbol, SymbolTable<Symbol, Entry>*, SymbolTable<Symbol, Signature>*);
};

bool match_signature(Signature*, Signature*);


#endif


