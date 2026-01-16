#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include <algorithm>
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

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   std::vector<CgenNode*> m_class_nodes;
   std::map<Symbol, int> m_class_tags;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   
   // Class table generation
   void code_class_nameTab();
   void code_class_objTab();
   void code_dispatchTabs();
   void code_protObjs();
   void code_class_inits();
   void code_class_methods();

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
   
   // Helper methods
   std::vector<CgenNode*> GetClassNodes();
   CgenNodeP GetClassNode(Symbol name);
   std::map<Symbol, int> GetClassTags();
   int GetIntTag() { return intclasstag; }
   int GetBoolTag() { return boolclasstag; }
   int GetStringTag() { return stringclasstag; }
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int class_tag;                             // Class tag for runtime
   std::vector<CgenNode*> inheritance;       // Inheritance chain
   std::vector<attr_class*> m_full_attribs;   // All attributes including inherited
   std::vector<method_class*> m_full_methods; // All methods including inherited
   std::map<Symbol, int> m_attrib_idx_tab;   // Attribute name to index
   std::map<Symbol, int> m_dispatch_idx_tab;  // Method name to dispatch index
   std::map<Symbol, Symbol> m_dispatch_class_tab; // Method name to defining class

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   
   // Class tag
   void SetClassTag(int tag) { class_tag = tag; }
   int GetClassTag() { return class_tag; }
   
   // Inheritance
   std::vector<CgenNode*> GetInheritance();
   
   // Attributes
   std::vector<attr_class*> GetAttribs();  // Current class only
   std::vector<attr_class*> GetFullAttribs();  // Including inherited
   std::map<Symbol, int> GetAttribIdxTab();
   
   // Methods
   std::vector<method_class*> GetMethods();  // Current class only
   std::vector<method_class*> GetFullMethods();  // Including inherited
   std::map<Symbol, int> GetDispatchIdxTab();
   std::map<Symbol, Symbol> GetDispatchClassTab();
   
   // Code generation
   void code_protObj(ostream& s);
   void code_init(ostream& s);
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

// Forward declaration
class CgenNode;

// Environment class for code generation
class Environment {
private:
  SymbolTable<Symbol, int> var_table;      // let variables
  SymbolTable<Symbol, int> param_table;   // method parameters
  std::vector<Symbol> var_stack;          // stack of variable scopes
  std::vector<Symbol> param_stack;         // stack of parameter scopes
  int var_offset;                          // current variable offset
  int param_offset;                       // current parameter offset
  
public:
  CgenNode* m_class_node;                  // current class node
  
  Environment() : var_offset(0), param_offset(0), m_class_node(NULL) {}
  
  // Variable management
  void EnterScope();
  void ExitScope();
  void AddVar(Symbol sym);
  int LookUpVar(Symbol sym);
  
  // Parameter management
  void AddParam(Symbol sym);
  int LookUpParam(Symbol sym);
  
  // Attribute lookup
  int LookUpAttrib(Symbol sym);
  
  // GC obstacle tracking
  void AddObstacle() {}  // placeholder for GC
};

// Global variable for codegen class table
extern CgenClassTable *codegen_classtable;
extern int labelnum;

