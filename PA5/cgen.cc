
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

// Global variables for code generation
CgenClassTable *codegen_classtable = NULL;
int labelnum = 0;

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
  codegen_classtable = new CgenClassTable(classes,os);

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
      emit_disptable_ref(Str, s);
      s << endl;                                              // dispatch table
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
      emit_disptable_ref(Int, s);
      s << endl;                                          // dispatch table
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
      emit_disptable_ref(Bool, s);
      s << endl;                                            // dispatch table
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
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
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
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
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


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   // Class tags: Object=0, IO=1, Int=2, Bool=3, String=4
   stringclasstag = 4;
   intclasstag = 2;
   boolclasstag = 3;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

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



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding class name table" << endl;
  code_class_nameTab();
  
  if (cgen_debug) cout << "coding class object table" << endl;
  code_class_objTab();
  
  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_dispatchTabs();
  
  if (cgen_debug) cout << "coding prototype objects" << endl;
  code_protObjs();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding class initializers" << endl;
  code_class_inits();
  
  if (cgen_debug) cout << "coding class methods" << endl;
  code_class_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

std::vector<CgenNode*> CgenClassTable::GetClassNodes() {
  if (m_class_nodes.empty()) {
    for (List<CgenNode> *l = nds; l; l = l->tl()) {
      m_class_nodes.push_back(l->hd());
    }
    std::reverse(m_class_nodes.begin(), m_class_nodes.end());
    
    // Assign class tags
    for (size_t i = 0; i < m_class_nodes.size(); ++i) {
      m_class_nodes[i]->SetClassTag(i);
      m_class_tags[m_class_nodes[i]->name] = i;
    }
  }
  return m_class_nodes;
}

CgenNodeP CgenClassTable::GetClassNode(Symbol name) {
  return probe(name);
}

std::map<Symbol, int> CgenClassTable::GetClassTags() {
  GetClassNodes();  // Ensure tags are assigned
  return m_class_tags;
}

void CgenClassTable::code_class_nameTab() {
  str << CLASSNAMETAB << LABEL;
  std::vector<CgenNode*> class_nodes = GetClassNodes();
  for (CgenNode* class_node : class_nodes) {
    Symbol class_name = class_node->name;
    StringEntry* str_entry = stringtable.lookup_string(class_name->get_string());
    str << WORD;
    str_entry->code_ref(str);
    str << endl;
  }
}

void CgenClassTable::code_class_objTab() {
  str << CLASSOBJTAB << LABEL;
  std::vector<CgenNode*> class_nodes = GetClassNodes();
  for (CgenNode* class_node : class_nodes) {
    str << WORD;
    emit_protobj_ref(class_node->name, str);
    str << endl;
    str << WORD;
    emit_init_ref(class_node->name, str);
    str << endl;
  }
}

void CgenClassTable::code_dispatchTabs() {
  std::vector<CgenNode*> class_nodes = GetClassNodes();
  for (CgenNode* class_node : class_nodes) {
    emit_disptable_ref(class_node->name, str);
    str << LABEL;
    std::vector<method_class*> methods = class_node->GetFullMethods();
    std::map<Symbol, Symbol> dispatch_class_tab = class_node->GetDispatchClassTab();
    for (method_class* method : methods) {
      Symbol method_name = method->name;
      Symbol defining_class = dispatch_class_tab[method_name];
      str << WORD;
      emit_method_ref(defining_class, method_name, str);
      str << endl;
    }
  }
}

void CgenClassTable::code_protObjs() {
  std::vector<CgenNode*> class_nodes = GetClassNodes();
  for (CgenNode* class_node : class_nodes) {
    class_node->code_protObj(str);
  }
}

void CgenClassTable::code_class_inits() {
  std::vector<CgenNode*> class_nodes = GetClassNodes();
  for (CgenNode* class_node : class_nodes) {
    class_node->code_init(str);
  }
}

void CgenClassTable::code_class_methods() {
  std::vector<CgenNode*> class_nodes = GetClassNodes();
  for (CgenNode* class_node : class_nodes) {
    std::vector<method_class*> methods = class_node->GetMethods();
    for (method_class* method : methods) {
      method->code(str, class_node);
    }
  }
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
   basic_status(bstatus),
   class_tag(-1)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

// Get inheritance chain from Object to this class
std::vector<CgenNode*> CgenNode::GetInheritance() {
  if (inheritance.empty()) {
    CgenNode* class_node = this;
    while (class_node->name != No_class) {
      inheritance.push_back(class_node);
      class_node = class_node->get_parentnd();
    }
    std::reverse(inheritance.begin(), inheritance.end());
  }
  return inheritance;
}

// Get attributes of current class only
std::vector<attr_class*> CgenNode::GetAttribs() {
  std::vector<attr_class*> ret;
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature feature = features->nth(i);
    attr_class* attr = dynamic_cast<attr_class*>(feature);
    if (attr) {
      ret.push_back(attr);
    }
  }
  return ret;
}

// Get all attributes including inherited
std::vector<attr_class*> CgenNode::GetFullAttribs() {
  if (m_full_attribs.empty()) {
    std::vector<CgenNode*> inheritance = GetInheritance();
    for (CgenNode* class_node : inheritance) {
      std::vector<attr_class*> attribs = class_node->GetAttribs();
      for (attr_class* attr : attribs) {
        m_full_attribs.push_back(attr);
      }
    }
    // Build index table
    for (size_t i = 0; i < m_full_attribs.size(); ++i) {
      m_attrib_idx_tab[m_full_attribs[i]->name] = i;
    }
  }
  return m_full_attribs;
}

std::map<Symbol, int> CgenNode::GetAttribIdxTab() {
  GetFullAttribs();  // Ensure it's built
  return m_attrib_idx_tab;
}

// Get methods of current class only
std::vector<method_class*> CgenNode::GetMethods() {
  std::vector<method_class*> ret;
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature feature = features->nth(i);
    method_class* method = dynamic_cast<method_class*>(feature);
    if (method) {
      ret.push_back(method);
    }
  }
  return ret;
}

// Get all methods including inherited (with override)
std::vector<method_class*> CgenNode::GetFullMethods() {
  if (m_full_methods.empty()) {
    std::vector<CgenNode*> inheritance = GetInheritance();
    for (CgenNode* class_node : inheritance) {
      std::vector<method_class*> methods = class_node->GetMethods();
      for (method_class* method : methods) {
        Symbol method_name = method->name;
        if (m_dispatch_idx_tab.find(method_name) == m_dispatch_idx_tab.end()) {
          // New method
          m_full_methods.push_back(method);
          m_dispatch_idx_tab[method_name] = m_full_methods.size() - 1;
          m_dispatch_class_tab[method_name] = class_node->name;
        } else {
          // Override - replace with subclass method
          int idx = m_dispatch_idx_tab[method_name];
          m_full_methods[idx] = method;
          m_dispatch_class_tab[method_name] = class_node->name;
        }
      }
    }
  }
  return m_full_methods;
}

std::map<Symbol, int> CgenNode::GetDispatchIdxTab() {
  GetFullMethods();  // Ensure it's built
  return m_dispatch_idx_tab;
}

std::map<Symbol, Symbol> CgenNode::GetDispatchClassTab() {
  GetFullMethods();  // Ensure it's built
  return m_dispatch_class_tab;
}

void CgenNode::code_protObj(ostream& s) {
  std::vector<attr_class*> attribs = GetFullAttribs();
  
  s << WORD << "-1" << endl;
  emit_protobj_ref(name, s);
  s << LABEL;
  s << WORD << class_tag << "\t# class tag" << endl;
  s << WORD << (DEFAULT_OBJFIELDS + attribs.size()) << "\t# size" << endl;
  s << WORD;
  emit_disptable_ref(name, s);
  s << endl;
  
  for (attr_class* attr : attribs) {
    if (attr->type_decl == Int) {
      s << WORD;
      inttable.lookup_string("0")->code_ref(s);
      s << "\t# int(0)" << endl;
    } else if (attr->type_decl == Bool) {
      s << WORD;
      falsebool.code_ref(s);
      s << "\t# bool(0)" << endl;
    } else if (attr->type_decl == Str) {
      s << WORD;
      stringtable.lookup_string("")->code_ref(s);
      s << "\t# str()" << endl;
    } else {
      s << WORD << "0\t# void" << endl;
    }
  }
}

void CgenNode::code_init(ostream& s) {
  emit_init_ref(name, s);
  s << LABEL;
  
  // Save registers
  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 4, s);
  emit_move(SELF, ACC, s);
  
  // Call parent init
  Symbol parent_name = get_parentnd()->name;
  if (parent_name != No_class) {
    s << "\t# init parent" << endl;
    s << JAL;
    emit_init_ref(parent_name, s);
    s << endl << endl;
  }
  
  // Initialize current class attributes
  std::vector<attr_class*> attribs = GetAttribs();
  std::map<Symbol, int> attrib_idx_tab = GetAttribIdxTab();
  
  for (attr_class* attr : attribs) {
    int idx = attrib_idx_tab[attr->name];
    if (attr->init->IsEmpty()) {
      if (attr->type_decl == Str) {
        emit_load_string(ACC, stringtable.lookup_string(""), s);
      } else if (attr->type_decl == Int) {
        emit_load_int(ACC, inttable.lookup_string("0"), s);
      } else if (attr->type_decl == Bool) {
        emit_load_bool(ACC, BoolConst(0), s);
      }
    } else {
      Environment env;
      env.m_class_node = this;
      attr->init->code(s, env);
    }
    emit_store(ACC, 3 + idx, SELF, s);
  }
  
  // Return self
  emit_move(ACC, SELF, s);
  
  // Restore registers
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  emit_return(s);
}

void method_class::code(ostream& s, CgenNode* class_node) {
  emit_method_ref(class_node->name, name, s);
  s << LABEL;
  
  // Save registers
  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 4, s);
  emit_move(SELF, ACC, s);
  
  // Setup environment
  Environment env;
  env.m_class_node = class_node;
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formal_class* formal = (formal_class*)formals->nth(i);
    env.AddParam(formal->name);
  }
  
  // Generate method body
  expr->code(s, env);
  
  // Restore registers
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  
  // Pop arguments
  int arg_num = GetArgNum();
  emit_addiu(SP, SP, arg_num * 4, s);
  
  emit_return(s);
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

void assign_class::code(ostream &s, Environment env) {
  s << "\t# Assign. First eval the expr." << endl;
  expr->code(s, env);

  s << "\t# Now find the lvalue." << endl;
  int idx;

  if ((idx = env.LookUpVar(name)) != -1) {
    s << "\t# It is a let variable." << endl;
    emit_store(ACC, idx + 1, SP, s);
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, SP, 4 * (idx + 1), s);
      emit_jal("_GenGC_Assign", s);
    }
  } else if ((idx = env.LookUpParam(name)) != -1){
    s << "\t# It is a param." << endl;
    emit_store(ACC, idx + 3, FP, s);
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, FP, 4 * (idx + 3), s);
      emit_jal("_GenGC_Assign", s);
    }
  } else if ((idx = env.LookUpAttrib(name)) != -1) {
    s << "\t# It is an attribute." << endl;
    emit_store(ACC, idx + 3, SELF, s);
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, SELF, 4 * (idx + 3), s);
      emit_jal("_GenGC_Assign", s);
    }
  } else {
    s << "Error! assign to what?" << endl;
  }
}

void static_dispatch_class::code(ostream &s, Environment env) {
  s << "\t# Static dispatch. First eval and save the params." << endl;
  std::vector<Expression> actuals = GetActuals();

  for (Expression expr : actuals) {
    expr->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
  }

  s << "\t# eval the obj in dispatch." << endl;
  expr->code(s, env);

  s << "\t# if obj = void: abort" << endl;
  emit_bne(ACC, ZERO, labelnum, s);
  s << LA << ACC << " str_const0" << endl;
  emit_load_imm(T1, 1, s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(labelnum, s);
  ++labelnum;

  // Use specified type
  Symbol _class_name = type_name;
  CgenNode* _class_node = codegen_classtable->GetClassNode(_class_name);
  
  s << "\t# Load dispatch table for " << type_name << endl;
  std::string addr = std::string(type_name->get_string()) + DISPTAB_SUFFIX;
  emit_load_address(T1, const_cast<char*>(addr.c_str()), s);
  
  int idx = _class_node->GetDispatchIdxTab()[name];
  emit_load(T1, idx, T1, s);
  
  emit_jalr(T1, s);
}

void dispatch_class::code(ostream &s, Environment env) {
  s << "\t# Dispatch. First eval and save the params." << endl;
  std::vector<Expression> actuals = GetActuals();

  for (Expression expr : actuals) {
    expr->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
  }

  s << "\t# eval the obj in dispatch." << endl;
  expr->code(s, env);

  s << "\t# if obj = void: abort" << endl;
  emit_bne(ACC, ZERO, labelnum, s);
  s << LA << ACC << " str_const0" << endl;
  emit_load_imm(T1, 1, s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(labelnum, s);
  ++labelnum;

  // Get current class name;
  Symbol _class_name = env.m_class_node->name;
  if (expr->get_type() != SELF_TYPE) {
    _class_name = expr->get_type();
  }

  CgenNode* _class_node = codegen_classtable->GetClassNode(_class_name);
  s << "\t# Now we locate the method in the dispatch table." << endl;
  s << "\t# t1 = self.dispTab" << endl;
  emit_load(T1, 2, ACC, s);
  s << endl;

  int idx = _class_node->GetDispatchIdxTab()[name];
  s << "\t# t1 = dispTab[offset]" << endl;
  emit_load(T1, idx, T1, s);
  s << endl;

  s << "\t# jumpto " << name << endl;
  emit_jalr(T1, s);
  s << endl;
}

void cond_class::code(ostream &s, Environment env) {
  s << "\t# If statement" << endl;
  pred->code(s, env);
  
  emit_load(T1, 3, ACC, s);  // Extract bool value
  
  int label_false = labelnum++;
  int label_finish = labelnum++;
  
  emit_beq(T1, ZERO, label_false, s);
  
  then_exp->code(s, env);
  emit_branch(label_finish, s);
  
  emit_label_def(label_false, s);
  else_exp->code(s, env);
  
  emit_label_def(label_finish, s);
}

void loop_class::code(ostream &s, Environment env) {
  s << "\t# While loop" << endl;
  int start = labelnum++;
  int finish = labelnum++;
  
  emit_label_def(start, s);
  
  pred->code(s, env);
  emit_load(T1, 3, ACC, s);
  
  emit_beq(T1, ZERO, finish, s);
  
  body->code(s, env);
  
  emit_branch(start, s);
  
  emit_label_def(finish, s);
  emit_move(ACC, ZERO, s);  // Loop returns void
}

void typcase_class::code(ostream &s, Environment env) {
  s << "\t# Case expression" << endl;
  expr->code(s, env);
  
  // Check if void
  emit_bne(ACC, ZERO, labelnum, s);
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, 1, s);
  emit_jal("_case_abort2", s);
  emit_label_def(labelnum, s);
  ++labelnum;
  
  emit_push(ACC, s);
  env.AddObstacle();
  
  std::vector<branch_class*> _cases = GetCases();
  int case_labels[_cases.size()];
  int finish_label = labelnum++;
  
  // Generate code for each case
  for (size_t i = 0; i < _cases.size(); ++i) {
    case_labels[i] = labelnum++;
    branch_class* _case = _cases[i];
    
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);  // Load object
    emit_load(T2, 0, T1, s);  // Load tag
    
    // Check if object is of this type
    Symbol _type = _case->type_decl;
    int _tag = codegen_classtable->GetClassTags()[_type];
    
    emit_load_imm(T3, _tag, s);
    emit_beq(T2, T3, case_labels[i], s);
    
    emit_push(T1, s);
  }
  
  // No match - abort
  emit_jal("_case_abort", s);
  
  // Generate code for each case branch
  for (size_t i = 0; i < _cases.size(); ++i) {
    emit_label_def(case_labels[i], s);
    branch_class* _case = _cases[i];
    
    env.EnterScope();
    env.AddVar(_case->name);
    
    _case->expr->code(s, env);
    
    emit_branch(finish_label, s);
    env.ExitScope();
  }
  
  emit_label_def(finish_label, s);
  emit_addiu(SP, SP, 4, s);
}

void block_class::code(ostream &s, Environment env) {
  s << "\t# Block expression" << endl;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s, env);
  }
}

void let_class::code(ostream &s, Environment env) {
  s << "\t# Let expression" << endl;
  
  // Evaluate initial value
  if (init->IsEmpty()) {
    // Use default value based on type
    if (type_decl == Str) {
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    } else if (type_decl == Int) {
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    } else if (type_decl == Bool) {
      emit_load_bool(ACC, BoolConst(0), s);
    } else {
      emit_move(ACC, ZERO, s);
    }
  } else {
    init->code(s, env);
  }
  
  emit_push(ACC, s);
  env.EnterScope();
  env.AddVar(identifier);
  
  body->code(s, env);
  
  emit_addiu(SP, SP, 4, s);
  env.ExitScope();
}

void plus_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Add" << endl;
  s << "\t# First eval e1 and push." << endl;
  e1->code(s, env);
  emit_push(ACC, s);
  env.AddObstacle();
  s << endl;

  s << "\t# Then eval e2 and make a copy for result." << endl;
  e2->code(s, env);
  emit_jal("Object.copy", s);
  s << endl;

  s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  s << endl;

  s << "\t# Extract the int inside the object." << endl;
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  s << endl;

  s << "\t# Modify the int inside t2." << endl;
  emit_add(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
  s << endl;
}

void sub_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Sub" << endl;
  e1->code(s, env);
  emit_push(ACC, s);
  env.AddObstacle();
  
  e2->code(s, env);
  emit_jal("Object.copy", s);
  
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  
  emit_sub(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void mul_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Mul" << endl;
  e1->code(s, env);
  emit_push(ACC, s);
  env.AddObstacle();
  
  e2->code(s, env);
  emit_jal("Object.copy", s);
  
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  
  emit_mul(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void divide_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Div" << endl;
  e1->code(s, env);
  emit_push(ACC, s);
  env.AddObstacle();
  
  e2->code(s, env);
  emit_jal("Object.copy", s);
  
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  
  emit_div(T3, T1, T2, s);
  emit_store(T3, 3, ACC, s);
}

void neg_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Neg" << endl;
  e1->code(s, env);
  emit_jal("Object.copy", s);
  
  emit_load(T1, 3, ACC, s);
  emit_neg(T1, T1, s);
  emit_store(T1, 3, ACC, s);
}

void lt_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Less than" << endl;
  e1->code(s, env);
  emit_push(ACC, s);
  env.AddObstacle();
  
  e2->code(s, env);
  
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  
  int label_true = labelnum++;
  int label_finish = labelnum++;
  
  emit_blt(T1, T2, label_true, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_branch(label_finish, s);
  
  emit_label_def(label_true, s);
  emit_load_bool(ACC, BoolConst(1), s);
  
  emit_label_def(label_finish, s);
}

void eq_class::code(ostream &s, Environment env) {
  s << "\t# Equality test" << endl;
  e1->code(s, env);
  emit_push(ACC, s);
  env.AddObstacle();
  
  e2->code(s, env);
  
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  
  // Check if both are Int, Bool, or String
  int label_true = labelnum++;
  int label_finish = labelnum++;
  
  // If same object, equal
  emit_beq(T1, T2, label_true, s);
  
  // If not same object, check if both are Int/Bool/String
  emit_load(T3, 0, T1, s);  // T3 = e1.tag
  emit_load(T2, 0, T2, s);  // T2 = e2.tag (overwrite T2)
  
  // If tags are different, not equal
  emit_bne(T3, T2, label_finish, s);
  
  // Check if Int, Bool, or String
  emit_load_imm(T2, codegen_classtable->GetIntTag(), s);
  emit_beq(T3, T2, label_true, s);
  emit_load_imm(T2, codegen_classtable->GetBoolTag(), s);
  emit_beq(T3, T2, label_true, s);
  emit_load_imm(T2, codegen_classtable->GetStringTag(), s);
  emit_beq(T3, T2, label_true, s);
  
  // For other types, not equal
  emit_branch(label_finish, s);
  
  emit_label_def(label_true, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_branch(label_finish, s);
  
  emit_label_def(label_finish, s);
  emit_load_bool(ACC, BoolConst(0), s);
}

void leq_class::code(ostream &s, Environment env) {
  s << "\t# Int operation : Less or equal" << endl;
  e1->code(s, env);
  emit_push(ACC, s);
  env.AddObstacle();
  
  e2->code(s, env);
  
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);
  
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  
  int label_true = labelnum++;
  int label_finish = labelnum++;
  
  emit_bleq(T1, T2, label_true, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_branch(label_finish, s);
  
  emit_label_def(label_true, s);
  emit_load_bool(ACC, BoolConst(1), s);
  
  emit_label_def(label_finish, s);
}

void comp_class::code(ostream &s, Environment env) {
  s << "\t# Boolean complement" << endl;
  e1->code(s, env);
  
  emit_load(T1, 3, ACC, s);
  
  int label_false = labelnum++;
  int label_finish = labelnum++;
  
  emit_beq(T1, ZERO, label_false, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_branch(label_finish, s);
  
  emit_label_def(label_false, s);
  emit_load_bool(ACC, BoolConst(1), s);
  
  emit_label_def(label_finish, s);
}

void int_const_class::code(ostream& s, Environment env)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, Environment env)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, Environment env)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, Environment env) {
  s << "\t# New expression" << endl;
  if (type_name == SELF_TYPE) {
    // SELF_TYPE需要运行时确定
    emit_load_address(T1, "class_objTab", s);
    emit_load(T2, 0, SELF, s);  // T2 = self的类标签
    emit_sll(T2, T2, 3, s);      // 乘以8（每个类在objTab中占2个字）
    emit_addu(T1, T1, T2, s);
    emit_push(T1, s);
    emit_load(ACC, 0, T1, s);    // 加载protObj
    emit_jal("Object.copy", s);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 1, T1, s);     // 加载init地址
    emit_jalr(T1, s);
  } else {
    // 静态类型，直接加载protObj
    std::string protobj = std::string(type_name->get_string()) + PROTOBJ_SUFFIX;
    emit_load_address(ACC, const_cast<char*>(protobj.c_str()), s);
    emit_jal("Object.copy", s);
    std::string init = std::string(type_name->get_string()) + CLASSINIT_SUFFIX;
    emit_jal(const_cast<char*>(init.c_str()), s);
  }
}

void isvoid_class::code(ostream &s, Environment env) {
  s << "\t# Isvoid test" << endl;
  e1->code(s, env);
  
  int label_void = labelnum++;
  int label_finish = labelnum++;
  
  emit_beq(ACC, ZERO, label_void, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_branch(label_finish, s);
  
  emit_label_def(label_void, s);
  emit_load_bool(ACC, BoolConst(1), s);
  
  emit_label_def(label_finish, s);
}

void no_expr_class::code(ostream &s, Environment env) {
  // No expression - do nothing or return void
  emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, Environment env) {
  int idx;
  
  // Check if it's self
  if (name == self) {
    emit_move(ACC, SELF, s);
  }
  // Check if it's a let variable
  else if ((idx = env.LookUpVar(name)) != -1) {
    emit_load(ACC, idx + 1, SP, s);
  }
  // Check if it's a parameter
  else if ((idx = env.LookUpParam(name)) != -1) {
    emit_load(ACC, idx + 3, FP, s);
  }
  // Check if it's an attribute
  else if ((idx = env.LookUpAttrib(name)) != -1) {
    emit_load(ACC, idx + 3, SELF, s);
  } else {
    s << "Error! undefined variable: " << name << endl;
  }
}

//////////////////////////////////////////////////////////////////////////////
//
// Environment class implementation
//
//////////////////////////////////////////////////////////////////////////////

void Environment::EnterScope() {
  var_table.enterscope();
  param_table.enterscope();
}

void Environment::ExitScope() {
  var_table.exitscope();
  param_table.exitscope();
}

void Environment::AddVar(Symbol sym) {
  var_table.addid(sym, new int(var_offset));
  var_offset++;
}

int Environment::LookUpVar(Symbol sym) {
  int *offset = var_table.lookup(sym);
  if (offset) {
    return *offset;
  }
  return -1;
}

void Environment::AddParam(Symbol sym) {
  param_table.addid(sym, new int(param_offset));
  param_offset++;
}

int Environment::LookUpParam(Symbol sym) {
  int *offset = param_table.lookup(sym);
  if (offset) {
    return *offset;
  }
  return -1;
}

int Environment::LookUpAttrib(Symbol sym) {
  if (!m_class_node) return -1;
  
  // Get all attributes including inherited ones
  std::vector<attr_class*> attribs = m_class_node->GetFullAttribs();
  std::map<Symbol, int> idx_tab = m_class_node->GetAttribIdxTab();
  
  if (idx_tab.find(sym) != idx_tab.end()) {
    return idx_tab[sym];
  }
  return -1;
}


