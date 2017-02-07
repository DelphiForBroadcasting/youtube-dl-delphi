unit js15decl;
interface

uses SysUtils, Windows;

const

{$ifdef cpux64}
    LibName =  'js64.dll';
{$else}
    LibName =  'js32.dll';
{$endif}

const

{$ifdef cpux64}
  JSVAL_TAG_SHIFT                = 47;
{$endif}

  // JSVALUE_TYPE
  JSVAL_TYPE_DOUBLE              = $00;
  JSVAL_TYPE_INT32               = $01;
  JSVAL_TYPE_UNDEFINED           = $02;
  JSVAL_TYPE_BOOLEAN             = $03;
  JSVAL_TYPE_MAGIC               = $04;
  JSVAL_TYPE_STRING              = $05;
  JSVAL_TYPE_NULL                = $06;
  JSVAL_TYPE_OBJECT              = $07;

  //* The below types never appear in a jsval; they are only used in tracing. */

  JSVAL_TYPE_NONFUNOBJ           = $57;
  JSVAL_TYPE_FUNOBJ              = $67;

  JSVAL_TYPE_STRORNULL           = $97;
  JSVAL_TYPE_OBJORNULL           = $98;

  JSVAL_TYPE_BOXED               = $99;

  JSCLASS_RESERVED_SLOTS_SHIFT  = 8;
  JSCLASS_RESERVED_SLOTS_MASK   = 255;
  JSCLASS_RESERVED_SLOTS_WIDTH  = 8;

  (* A couple static values for reserving slots in a custom JSClass *)
  JSCLASS_RESERVE_SLOTS_1   = 512;
  JSCLASS_RESERVE_SLOTS_2   = 1024;

  (* Pretty near all of these are enums within the API *)
  JSVAL_OBJECT = $0;
  JSVAL_INT = $1;
  JSVAL_DOUBLE = $2;
  JSVAL_STRING = $4;
  JSVAL_BOOLEAN = $6;
  JSVAL_TAGMASK = $1 or $2 or $4;
  JSVAL_TAGBITS = 3;

  (* Flags for JSClass |flags| property *)
  JSCLASS_HAS_PRIVATE = 1 shl 0;
  JSCLASS_NEW_ENUMERATE = 1 shl 1;
  JSCLASS_NEW_RESOLVE = 1 shl 2;
  JSCLASS_PRIVATE_IS_NSISUPPORTS = 1 shl 3;
  JSCLASS_SHARE_ALL_PROPERTIES = 1 shl 4;
  JSCLASS_NEW_RESOLVE_GETS_START = 1 shl 5;

  JSCLASS_HIGH_FLAGS_SHIFT  = JSCLASS_RESERVED_SLOTS_SHIFT + JSCLASS_RESERVED_SLOTS_WIDTH;
  JSCLASS_IS_EXTENDED       = 1 shl (JSCLASS_HIGH_FLAGS_SHIFT+0);
  JSCLASS_IS_ANONYMOUS      = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+1));
  JSCLASS_IS_GLOBAL         = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+2));


  JSCLASS_GLOBAL_FLAGS      = (JSCLASS_IS_GLOBAL);// or JSCLASS_HAS_RESERVED_SLOTS(JSRESERVED_GLOBAL_THIS + JSRESERVED_GLOBAL_SLOTS_COUNT));

  (* May be private *)
  JS_MAP_GCROOT_NEXT = 0;
  JS_MAP_GCROOT_STOP = 1;
  JS_MAP_GCROOT_REMOVE = 2;

  (* May be private *)
  JSOPTION_STRICT                 = 1 shl 0;
  JSOPTION_WERROR                 = 1 shl 1;
  JSOPTION_VAROBJFIX              = 1 shl 2;
  JSOPTION_PRIVATE_IS_NSISUPPORTS = 1 shl 3;
  JSOPTION_COMPILE_N_GO           = 1 shl 4;
  JSOPTION_ATLINE                 = 1 shl 5;
  JSOPTION_XML                    = 1 shl 6;
  JSOPTION_NATIVE_BRANCH_CALLBACK = 1 shl 7;
  JSOPTION_DONT_REPORT_UNCAUGHT   = 1 shl 8;
  JSOPTION_JIT                    = 1 shl 11;
  JSOPTION_METHODJIT              = 1 shl 14;
  JSOPTION_PROFILING              = 1 shl 15;
  JSOPTION_METHODJIT_ALWAYS       = 1 shl 16;

  (* Numeric equivalents of javascript versions *)

  (* Property attributes *)
  JSPROP_ENUMERATE = $01;
  JSPROP_READONLY = $02;
  JSPROP_PERMANENT = $04;
  JSPROP_GETTER = $10;
  JSPROP_SETTER = $20;
  JSPROP_SHARED = $40;
  JSPROP_INDEX = $80;
  JSPROP_SHORTID = $100;

  (* Function attributes *)
  JSFUN_LAMBDA = $08;
  JSFUN_GETTER = JSPROP_GETTER;
  JSFUN_SETTER = JSPROP_SETTER;
  JSFUN_BOUND_METHOD = $40;
  JSFUN_HEAVYWEIGHT = $80;
{$ifdef MOZILLA_1_8_BRANCH}
  JSFUN_FLAGS_MASK = $F8;
{$else}
  JSFUN_FLAGS_MASK = $0fF8;
{$endif}

  (*  *)
  JSPD_ENUMERATE = $01;
  JSPD_READONLY = $02;
  JSPD_PERMANENT = $04;
  JSPD_ALIAS = $08;
  JSPD_ARGUMENT = $10;
  JSPD_VARIABLE = $20;
  JSPD_EXCEPTION = $40;
  JSPD_ERROR = $80;

  (* De/serialization modes *)
  JSXDR_ENCODE = 0;
  JSXDR_DECODE = 1;
  JSXDR_FREE = 2;

  (* De/serialization stream positions *)
  JSXDR_SEEK_SET = 0;
  JSXDR_SEEK_CUR = 1;
  JSXDR_SEEK_END = 2;

  (* Error report flag values *)
  JSREPORT_ERROR = $0;
  JSREPORT_WARNING = $1;
  JSREPORT_EXCEPTION = $2;
  JSREPORT_STRICT = $4;

  (* An example of an enumerated type that needs to be constant, but...well, see below.
  JSACC_PROTO = 0;
  JSACC_PARENT = 1;
  JSACC_IMPORT = 2;
  JSACC_WATCH = 3;
  JSACC_READ = 4;
  JSACC_WRITE = 8;
  *)

type
  (* These are OK as enums, since the values are sequential from 0 *)
  JSType = (
    JSTYPE_VOID,                // undefined */
    JSTYPE_OBJECT,              // object */
    JSTYPE_FUNCTION,            // function */
    JSTYPE_STRING,              // string */
    JSTYPE_NUMBER,              // number */
    JSTYPE_BOOLEAN,             // boolean */
    JSTYPE_NULL,                // null */
    JSTYPE_XML,                 // xml object */
    JSTYPE_LIMIT
  );
  JSIterateOp = (JSENUMERATE_INIT, JSENUMERATE_NEXT, JSENUMERATE_DESTROY);
  JSGCStatus = (JSGC_BEGIN, JSGC_END, JSGC_MARK_END, JSGC_FINALIZE_END);

  JSOp = (JSOP_NOP,
JSOP_PUSH,
JSOP_POPV,
JSOP_ENTERWITH,
JSOP_LEAVEWITH,
JSOP_RETURN,
JSOP_GOTO,
JSOP_IFEQ,
JSOP_IFNE,
JSOP_ARGUMENTS,
JSOP_FORARG,
JSOP_FORVAR,
JSOP_DUP,
JSOP_DUP2,
JSOP_SETCONST,
JSOP_BITOR,
JSOP_BITXOR,
JSOP_BITAND,
JSOP_EQ,
JSOP_NE,
JSOP_LT,
JSOP_LE,
JSOP_GT,
JSOP_GE,
JSOP_LSH,
JSOP_RSH,
JSOP_URSH,
JSOP_ADD,
JSOP_SUB,
JSOP_MUL,
JSOP_DIV,
JSOP_MOD,
JSOP_NOT,
JSOP_BITNOT,
JSOP_NEG,
JSOP_NEW,
JSOP_DELNAME,
JSOP_DELPROP,
JSOP_DELELEM,
JSOP_TYPEOF,
JSOP_VOID,
JSOP_INCNAME,
JSOP_INCPROP,
JSOP_INCELEM,
JSOP_DECNAME,
JSOP_DECPROP,
JSOP_DECELEM,
JSOP_NAMEINC,
JSOP_PROPINC,
JSOP_ELEMINC,
JSOP_NAMEDEC,
JSOP_PROPDEC,
JSOP_ELEMDEC,
JSOP_GETPROP,
JSOP_SETPROP,
JSOP_GETELEM,
JSOP_SETELEM,
JSOP_PUSHOBJ,
JSOP_CALL,
JSOP_NAME,
JSOP_NUMBER,
JSOP_STRING,
JSOP_ZERO,
JSOP_ONE,
JSOP_NULL,
JSOP_THIS,
JSOP_FALSE,
JSOP_TRUE,
JSOP_OR,
JSOP_AND,
JSOP_TABLESWITCH,
JSOP_LOOKUPSWITCH,
JSOP_NEW_EQ,
JSOP_NEW_NE,
JSOP_CLOSURE,
JSOP_EXPORTALL,
JSOP_EXPORTNAME,
JSOP_IMPORTALL,
JSOP_IMPORTPROP,
JSOP_IMPORTELEM,
JSOP_OBJECT,
JSOP_POP,
JSOP_POS,
JSOP_TRAP,
JSOP_GETARG,
JSOP_SETARG,
JSOP_GETVAR,
JSOP_SETVAR,
JSOP_UINT16,
JSOP_NEWINIT,
JSOP_ENDINIT,
JSOP_INITPROP,
JSOP_INITELEM,
JSOP_DEFSHARP,
JSOP_USESHARP,
JSOP_INCARG,
JSOP_INCVAR,
JSOP_DECARG,
JSOP_DECVAR,
JSOP_ARGINC,
JSOP_VARINC,
JSOP_ARGDEC,
JSOP_VARDEC,
JSOP_FORIN,
JSOP_FORNAME,
JSOP_FORPROP,
JSOP_FORELEM,
JSOP_POP2,
JSOP_BINDNAME,
JSOP_SETNAME,
JSOP_THROW,
JSOP_IN,
JSOP_INSTANCEOF,
JSOP_DEBUGGER,
JSOP_GOSUB,
JSOP_RETSUB,
JSOP_EXCEPTION,
JSOP_SETSP,
JSOP_CONDSWITCH,
JSOP_CASE,
JSOP_DEFAULT,
JSOP_EVAL,
JSOP_ENUMELEM,
JSOP_GETTER,
JSOP_SETTER,
JSOP_DEFFUN,
JSOP_DEFCONST,
JSOP_DEFVAR,
JSOP_ANONFUNOBJ,
JSOP_NAMEDFUNOBJ,
JSOP_SETLOCALPOP,
JSOP_GROUP,
JSOP_SETCALL,
JSOP_TRY,
JSOP_FINALLY,
JSOP_SWAP,
JSOP_ARGSUB,
JSOP_ARGCNT,
JSOP_DEFLOCALFUN,
JSOP_GOTOX,
JSOP_IFEQX,
JSOP_IFNEX,
JSOP_ORX,
JSOP_ANDX,
JSOP_GOSUBX,
JSOP_CASEX,
JSOP_DEFAULTX,
JSOP_TABLESWITCHX,
JSOP_LOOKUPSWITCHX,
JSOP_BACKPATCH,
JSOP_BACKPATCH_POP,
JSOP_THROWING,
JSOP_SETRVAL,
JSOP_RETRVAL,
JSOP_GETGVAR,
JSOP_SETGVAR,
JSOP_INCGVAR,
JSOP_DECGVAR,
JSOP_GVARINC,
JSOP_GVARDEC,
JSOP_REGEXP,
JSOP_DEFXMLNS,
JSOP_ANYNAME,
JSOP_QNAMEPART,
JSOP_QNAMECONST,
JSOP_QNAME,
JSOP_TOATTRNAME,
JSOP_TOATTRVAL,
JSOP_ADDATTRNAME,
JSOP_ADDATTRVAL,
JSOP_BINDXMLNAME,
JSOP_SETXMLNAME,
JSOP_XMLNAME,
JSOP_DESCENDANTS,
JSOP_FILTER,
JSOP_ENDFILTER,
JSOP_TOXML,
JSOP_TOXMLLIST,
JSOP_XMLTAGEXPR,
JSOP_XMLELTEXPR,
JSOP_XMLOBJECT,
JSOP_XMLCDATA,
JSOP_XMLCOMMENT,
JSOP_XMLPI,
JSOP_GETMETHOD,
JSOP_GETFUNNS,
JSOP_FOREACH,
JSOP_DELDESC,
JSOP_UINT24,
JSOP_LITERAL,
JSOP_FINDNAME,
JSOP_LITOPX,
JSOP_STARTXML,
JSOP_STARTXMLEXPR,
JSOP_SETMETHOD,
JSOP_STOP,
JSOP_GETXPROP,
JSOP_GETXELEM,
JSOP_TYPEOFEXPR,
JSOP_ENTERBLOCK,
JSOP_LEAVEBLOCK,
JSOP_GETLOCAL,
JSOP_SETLOCAL,
JSOP_INCLOCAL,
JSOP_DECLOCAL,
JSOP_LOCALINC,
JSOP_LOCALDEC,
JSOP_FORLOCAL,
JSOP_STARTITER,
JSOP_ENDITER,
JSOP_GENERATOR,
JSOP_YIELD,
JSOP_ARRAYPUSH,
JSOP_FOREACHKEYVAL,
JSOP_ENUMCONSTELEM,
JSOP_LEAVEBLOCKEXPR,
JSOP_LIMIT
);
  (* These need to move out as constants, but they're also "types" in that one or more APIs want a
   * specific range of values.  These are not sequential values, and I don't really know a decent
   * balance between the two situations.  (See above commented const declaration. *)
  JSAccessMode = (JSACC_PROTO = 0, JSACC_PARENT = 1, JSACC_WATCH =3, JSACC_READ = 4, JSACC_WRITE = 8, JSACC_LIMIT);
  JSExecPart = (JSEXEC_PROLOG, JSEXEC_MAIN);
  JSTrapStatus = (JSTRAP_ERROR, JSTRAP_CONTINUE , JSTRAP_RETURN, JSTRAP_THROW, JSTRAP_LIMIT);

  size_t = LongWord;
  ptrdiff_t = nativeInt;
  uptrdiff_t = nativeUInt;
//  int8 = ShortInt;
//  uint8 = Byte;
  uintN = LongWord;
  intN = integer;
//  uint32 = LongWord;
//  int32 = Integer;
//  uint16 = Word;
//  int16 = SmallInt;

  puintN = ^uintN;
  pintN = ^intN;
  puint32 = ^uint32;
  pint32 = ^int32;
  puint16 = ^uint16;
  pint16 = ^int16;
  pint8 = ^int8;
  puint8 = ^uint8;

  JSUintn = LongWord;
  JSIntn = Integer;
  JSUint8 = Byte;
  JSInt8 = ShortInt;
  JSUint16 = Word;
  JSInt16 = SmallInt;
  JSUint32 = LongWord;
  JSInt32 = Integer;
  JSUint64 = UInt64; // no such thing as unsigned 64-bit numbers in Delphi 5!
  JSInt64 = Int64;
  JSFloat32 = Single;
  JSFloat64 = Double;

  JSWord = NativeInt;
  JSUWord = NativeUInt;

  JSBool = JSIntn;
  JSPackedBool = JSUint8;
  JSSize = size_t;
  JSPtrdiff = ptrdiff_t;
  JSUptrdiff = uptrdiff_t;
//  JSVersion = Integer;
  JSVersion = (
    JSVERSION_1_0     = 100,
    JSVERSION_1_1     = 110,
    JSVERSION_1_2     = 120,
    JSVERSION_1_3     = 130,
    JSVERSION_1_4     = 140,
    JSVERSION_ECMA_3  = 148,
    JSVERSION_1_5     = 150,
    JSVERSION_1_6     = 160,
    JSVERSION_1_7     = 170,
    JSVERSION_1_8     = 180,
    JSVERSION_ECMA_5  = 185,
    JSVERSION_DEFAULT = 0,
    JSVERSION_UNKNOWN = -1,
    JSVERSION_LATEST  = JSVERSION_ECMA_5
  );
  JSHashNumber = uint32;
  JSXDRMode = uint32;

  jschar = WideChar;
  jsint = JSInt32;
  jsuint = JSUint32;
  jsdouble = JSFloat64;

  pjschar = PWideChar;
  jsid = size_t;

  TJSContextType = uint8;
  TJSScriptType = uint8;
  TJSRuntimeType = uint8;
  TJSObjectType = uint8;
  TJSPrincipalsType = uint8;


  PJSString = ^JSString;
  TJSString0 = record
    case integer of
      0: (chars: pjschar);
      1: (left: PJSString);
  end;

  TJSString1 = record
    case integer of
      0: (right: pjsstring);
      1: (base: PJSString);
      2: (capacity: size_t);
  end;
  TJSString2 = record
    case integer of
      0: (parent: pjsstring);
      1: (reserved: size_t);
  end;

  TJSString3 = record
    u0: TJSString1;
    u1: TJSString2;
  end;

  JSString = record
    lengthAndFlags: size_t;
    u: TJSString0;

    case integer of
     0: (inlineStorage: array[0..3] of jschar);
     1: (s: TJSString3);
     2: (externalStringType: size_t);
  end;

  JSIdArray = record
    length: jsint;
    vector: jsid;
  end;

  PPJSString = ^PJSString;
  PJSObject = ^TJSObjectType; // ^JSObject;
  PPJSObject = ^PJSObject;

{$ifdef cpux64}
  TJSValueTag = (
//const
    JSVAL_TAG_MAX_DOUBLE           = $1FFF0,
    JSVAL_TAG_INT32                = uint32(JSVAL_TAG_MAX_DOUBLE) or JSVAL_TYPE_INT32,
    JSVAL_TAG_UNDEFINED            = uint32(JSVAL_TAG_MAX_DOUBLE) or JSVAL_TYPE_UNDEFINED,
    JSVAL_TAG_STRING               = uint32(JSVAL_TAG_MAX_DOUBLE)  or JSVAL_TYPE_STRING,
    JSVAL_TAG_BOOLEAN              = uint32(JSVAL_TAG_MAX_DOUBLE)  or JSVAL_TYPE_BOOLEAN,
    JSVAL_TAG_MAGIC                = uint32(JSVAL_TAG_MAX_DOUBLE) or JSVAL_TYPE_MAGIC,
    JSVAL_TAG_NULL                 = uint32(JSVAL_TAG_MAX_DOUBLE) or JSVAL_TYPE_NULL,
    JSVAL_TAG_OBJECT               = uint32(JSVAL_TAG_MAX_DOUBLE) or JSVAL_TYPE_OBJECT
  );

Const
  JSVAL_PAYLOAD_MASK:uint64 =           $00007FFFFFFFFFFF;
  JSVAL_TAG_MASK:uint64 =               $FFFF800000000000;

  JSVAL_SHIFTED_TAG_MAX_DOUBLE   = (uint64(JSVAL_TAG_MAX_DOUBLE) shl JSVAL_TAG_SHIFT) or $FFFFFFFF;
  JSVAL_SHIFTED_TAG_INT32        = (uint64(JSVAL_TAG_INT32)      shl JSVAL_TAG_SHIFT);
  JSVAL_SHIFTED_TAG_UNDEFINED    = (uint64(JSVAL_TAG_UNDEFINED)  shl JSVAL_TAG_SHIFT);
  JSVAL_SHIFTED_TAG_STRING       = (uint64(JSVAL_TAG_STRING)     shl JSVAL_TAG_SHIFT);
  JSVAL_SHIFTED_TAG_BOOLEAN      = (uint64(JSVAL_TAG_BOOLEAN)    shl JSVAL_TAG_SHIFT);
  JSVAL_SHIFTED_TAG_MAGIC        = (uint64(JSVAL_TAG_MAGIC)      shl JSVAL_TAG_SHIFT);
  JSVAL_SHIFTED_TAG_NULL         = (uint64(JSVAL_TAG_NULL)       shl JSVAL_TAG_SHIFT);
  JSVAL_SHIFTED_TAG_OBJECT       = (uint64(JSVAL_TAG_OBJECT)     shl JSVAL_TAG_SHIFT);

  JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_PRIMITIVE_SET    = JSVAL_SHIFTED_TAG_OBJECT;
{$else}
  TJSValueTag = (
//const
    JSVAL_TAG_CLEAR                = $FFFF0000,
    JSVAL_TAG_INT32                = uint32(JSVAL_TAG_CLEAR) or JSVAL_TYPE_INT32,
    JSVAL_TAG_UNDEFINED            = uint32(JSVAL_TAG_CLEAR) or JSVAL_TYPE_UNDEFINED,
    JSVAL_TAG_STRING               = uint32(JSVAL_TAG_CLEAR) or JSVAL_TYPE_STRING,
    JSVAL_TAG_BOOLEAN              = uint32(JSVAL_TAG_CLEAR) or JSVAL_TYPE_BOOLEAN,
    JSVAL_TAG_MAGIC                = uint32(JSVAL_TAG_CLEAR) or JSVAL_TYPE_MAGIC,
    JSVAL_TAG_NULL                 = uint32(JSVAL_TAG_CLEAR) or JSVAL_TYPE_NULL,
    JSVAL_TAG_OBJECT               = uint32(JSVAL_TAG_CLEAR) or JSVAL_TYPE_OBJECT
  );
{$endif}

type
 JSWhyMagic = (
    JS_ARRAY_HOLE,               //* a hole in a dense array */
    JS_ARGS_HOLE,                //* a hole in the args object's array */
    JS_NATIVE_ENUMERATE,         //* indicates that a custom enumerate hook forwarded
                                 // * to js_Enumerate, which really means the object can be
                                 // * enumerated like a native object. */
    JS_NO_ITER_VALUE,            //* there is not a pending iterator value */
    JS_GENERATOR_CLOSING,        //* exception value thrown when closing a generator */
    JS_NO_CONSTANT,              //* compiler sentinel value */
    JS_THIS_POISON,              //* used in debug builds to catch tracing errors */
    JS_ARG_POISON,               //* used in debug builds to catch tracing errors */
    JS_SERIALIZE_NO_NODE,        //* an empty subnode in the AST serializer */
    JS_GENERIC_MAGIC             //* for local use */
  );

{$ifdef CPUX64}
  Tpayload = record
    case Integer of
      0: (i32: int32;);
      1: (u32: uint32;);
      2: (why: JSWhyMagic;);
      3: (word:jsuword;);
  end;


  jsval_layout = record
 { public
    class operator Implicit(const Value: boolean): jsval_layout;
    class operator Implicit(const Value: Integer): jsval_layout;
    class operator Implicit(const Value: Double): jsval_layout;
    class operator Implicit(const Value: Int64): jsval_layout;
  }  case integer of
      0: (asBits: UInt64);
      1: (payload: Tpayload;
        //  tag: TJSValueTag;
         );
      2: (asDouble: double;);
      3: (asPtr: pointer;);
  end;
{$else}
  Tpayload = record
    case Integer of
      0: (i32: int32;);
      1: (u32: uint32;);
      2: (boo: JSBool;);
      3: (str: PJSString ; );
      4: (obj: PJSObject;);
      5: (ptr: pointer;);
      6: (why: JSWhyMagic;);
      7: (word:jsuword;);
  end;


  jsval_layout = record
{  public
    class operator Implicit(const Value: boolean): jsval_layout;
    class operator Implicit(const Value: Integer): jsval_layout;
    class operator Implicit(const Value: Double): jsval_layout;
    class operator Implicit(const Value: Int64): jsval_layout;
 }  case integer of
      0: (asBits: UInt64);
      1: (payload: Tpayload;
          tag: TJSValueTag;
         );
      2: (asDouble: double;);
      3: (asPtr: pointer;);
  end;
{$endif}

{$IF CompilerVersion >= 22}
{$ifend}

  jsval = jsval_layout;

  jsrefcount = JSInt32;
  jsbytecode = uint8;
  jsatomid = uint32;

  ppjschar = ^pjschar;
  pjsint = ^jsint;
  pjsuint = ^jsuint;
  pjsdouble = ^jsdouble;
  ppjsval = ^pjsval;
  pjsval = ^jsval;
  pjsid = ^jsid;
  pjsrefcount = ^jsrefcount;
  pjsbytecode = ^jsbytecode;

  PJSBool = ^JSBool;

const
  JS_TRUE: JSIntn = 1;
  JS_FALSE: JSIntn = 0;

  JSVAL_LOWER_INCL_TAG_OF_OBJ_OR_NULL_SET         = JSVAL_TAG_NULL ;
  JSVAL_UPPER_EXCL_TAG_OF_PRIMITIVE_SET           = JSVAL_TAG_OBJECT;
  JSVAL_UPPER_INCL_TAG_OF_NUMBER_SET              = JSVAL_TAG_INT32;
  JSVAL_LOWER_INCL_TAG_OF_GCTHING_SET             = JSVAL_TAG_STRING;

//     ((((uint64)(uint32)(tag)) << 32) | (uint32)(payload))
var
{$ifdef cpux64}
  JSVAL_VOID: jsval = (asBits:  uint64(uint64(JSVAL_TAG_UNDEFINED) shl JSVAL_TAG_SHIFT ) or 0);
  JSVAL_NULL: jsval = (asBits: uint64(uint64(JSVAL_TAG_NULL) shl JSVAL_TAG_SHIFT )or 0);
  JSVAL_ZERO: jsval = (asBits: uint64(uint64(JSVAL_TAG_INT32) shl JSVAL_TAG_SHIFT ) or 0);
  JSVAL_ONE: jsval = (asBits: uint64(uint64(JSVAL_TAG_INT32) shl JSVAL_TAG_SHIFT ) or 1);
  JSVAL_FALSE: jsval = (asBits: uint64(uint64(JSVAL_TAG_BOOLEAN) shl JSVAL_TAG_SHIFT ) or 0);
  JSVAL_TRUE: jsval = (asBits: uint64(uint64(JSVAL_TAG_BOOLEAN) shl JSVAL_TAG_SHIFT ) or 1);
{$else}
  JSVAL_VOID: jsval = (asBits:  uint64(uint64(JSVAL_TAG_UNDEFINED) shl 32 ) or 0);
  JSVAL_NULL: jsval = (asBits: uint64(uint64(JSVAL_TAG_NULL) shl 32 )or 0);
  JSVAL_ZERO: jsval = (asBits: uint64(uint64(JSVAL_TAG_INT32) shl 32 ) or 0);
  JSVAL_ONE: jsval = (asBits: uint64(uint64(JSVAL_TAG_INT32) shl 32 ) or 1);
  JSVAL_FALSE: jsval = (asBits: uint64(uint64(JSVAL_TAG_BOOLEAN) shl 32 ) or 0);
  JSVAL_TRUE: jsval = (asBits: uint64(uint64(JSVAL_TAG_BOOLEAN) shl 32 ) or 1);
{$endif}

type
  PFile = ^File;
  (* Some of the following are intended to be opaque pointers, others accessible.  Haven't sorted out which, though. *)
  PJSClass = ^JSClass;

  PJSContext = ^TJSContextType;
  PJSRuntime = ^TJSRuntimeType;
  PJSErrorReport = ^JSErrorReport;
  PJSIdArray = ^JSIdArray;
  PJSPrincipals = ^TJSPrincipalsType;
  PJSFunction = ^JSFunction;
  PJSFunctionSpec = ^JSFunctionSpec;
  PJSLocaleCallbacks = ^JSLocaleCallbacks;
  PJSExceptionState = ^JSExceptionState;
  PJSHashTable = Pointer;
  PPJSHashEntry = ^PJSHashEntry;
  PJSHashEntry = Pointer;
  PJSConstDoubleSpec = ^JSConstDoubleSpec;
  PJSPropertySpec = ^JSPropertySpec;
  PJSPropertyDesc = ^JSPropertyDesc;
  PJSPropertyDescArray = ^JSPropertyDescArray;
  PJSScopeProperty = ^JSScopeProperty;
  PJSStackFrame = pointer;
  PJSAtom = Pointer; // ^JSAtom
  PPJSAtom = ^PJSAtom;
  PJSObjectMap = ^JSObjectMap;
  PJSObjectOps = ^JSObjectOps;
  PJSProperty = ^JSProperty;
  PJSXDRState = ^JSXDRState;
  JSXDRState = Pointer;

  JSStackFrame = record
    callobj: PJSObject;       //lazily created Call object */
    argsobj: PJSObject;       //lazily created arguments object */
    varobj: PJSObject;        //variables object, where vars go */
    script: PJSObject;        //script being interpreted */
    fun: PJSFunction;           //function being called or null */
    thisp: PJSObject;         //"this" pointer if in method */
    argc: uintN;           //actual argument count */
    argv: pjsval;          //base of argument stack slots */
    rval: jsval;           //function return value */
    nvars: uintN;          //local variable count */
    vars: Pjsval;          //base of variable stack slots */
    down: PJSStackFrame;          //previous frame */
    annotation: pointer;    //used by Java security */
    scopeChain: PJSObject;    //scope chain */
    pc: Pjsbytecode;            //program counter */
    sp: pjsval;            //stack pointer */
    spbase: pjsval;        //operand stack base */
    sharpDepth: uintN;     //array/object initializer depth */
    sharpArray: pjsobject;    //scope for #n= initializer vars */
    flags: uint32          ;          //frame flags -- see below */
    dormantNext: PJSStackFrame;   //next dormant frame chain */
    xmlNamespace: PJSObject;  //null or default xml namespace in E4X */
    blockChain: PJSObject;    //active compile-time block scopes */
  end;

{
typedef JSBool
(* JS_DLL_CALLBACK JSEqualityOp)(JSContext *cx, JSObject *obj, jsval v,
                                 JSBool *bp);
}


{typedef JSTrapStatus
(* JS_DLL_CALLBACK JSTrapHandler)(JSContext *cx, JSScript *script,
                                  jsbytecode *pc, jsval *rval, void *closure);

}
  JSTrapHandler = function(cx: PJSContext; script: PJSObject; pc: pjsbytecode; rval: pjsval; closure: pointer): JSTrapStatus; cdecl;
  JSWatchPointHandler = function (cx: PJSContext; obj: PJSObject; id, old: jsval; newp: pjsval; closure: pointer): JSBOOL; cdecl;
  JSNewScriptHook = procedure (cx: pJSContext;
                                    filename: PAnsiChar; //* URL of script
                                    lineno: uintN;     //* first line */
                                    script: PJSObject;
                                    fun: PJSFunction;
                                    callerdata: pointer); cdecl;

 //* called just before script destruction */
 JSDestroyScriptHook = procedure(cx: pJSContext; script: PJSObject; callerdata: pointer); cdecl;

 JSSourceHandler = procedure (filename: PAnsiChar; lineno: uintN;
                                    str: pjschar; length: size_t;
                                    var listenerTSData: pointer; closure: pointer);



 JSInterpreterHook = function (cx: pJSContext;fp: pJSStackFrame;  before: JSBool;
                                      ok: PJSBool; closure: pointer): pointer; cdecl;

 JSObjectHook = procedure (cx: pJSContext; obj: pJSObject;  isNew: JSBool; closure: pointer); cdecl;

 JSDebugErrorHook = function (cx: pJSContext; const _message: pansichar;
                                     report: PJSErrorReport; closure: pointer): JSBool; cdecl;



  JSEqualityOp  = function(cx: PJSContext; obj: PJSObject; v: jsval; var bp: JSBool): JSBool; cdecl;
  JSPropertyOp = function(cx: PJSContext; obj: PJSObject; id: jsid; vp: pjsval): JSBool; cdecl;
  JSStrictPropertyOp = function(cx: PJSContext; obj: PJSObject; id: jsid; _strict: jsbool; vp: pjsval): JSBool; cdecl;
  JSNewEnumerateOp = function(cx: PJSContext; obj: PJSObject; enum_op: JSIterateOp; statep: pjsval; idp: pjsid): JSBool; cdecl;
  JSEnumerateOp = function(cx: PJSContext; obj: PJSObject): JSBool; cdecl;
  JSResolveOp = function(cx: PJSContext; obj: PJSObject; id: jsid): JSBool; cdecl;
  JSConvertOp = function(cx: PJSContext; obj: PJSObject; typ: JSType; vp: pjsval): JSBool; cdecl;
  JSFinalizeOp = procedure(cx: PJSContext; obj: PJSObject); cdecl;

  JSGetObjectOps = function(cx: PJSContext; clasp: PJSClass): Pointer; cdecl;
  JSCheckAccessOp = function(cx: PJSContext; obj: PJSObject; id: jsid; mode: JSAccessMode; vp: pjsval): JSBool; cdecl;
  JSXDRObjectOp = function(xdr: PJSXDRState; objp: PPJSObject): JSBool; cdecl;
  JSHasInstanceOp = function(cx: PJSContext; obj: PJSObject; v: jsval; bp: PJSBool): JSBool; cdecl;
  JSMarkOp = function(cx: PJSContext; obj: PJSObject; arg: Pointer): uint32; cdecl;
  JSNewObjectMapOp = function(cx: PJSContext; nrefs: jsrefcount; ops: PJSObjectOps; clasp: PJSClass; obj: PJSObject): PJSObjectMap; cdecl;
  JSObjectMapOp = procedure(cx: PJSContext; map: PJSObjectMap); cdecl;
  JSLookupPropOp = function(cx: PJSContext; obj: PJSObject; id: jsid; var objp: PJSObject; var propp: PJSProperty): JSBool; cdecl;
  JSDefinePropOp = function(cx: PJSContext; obj: PJSObject; id: jsid; value: jsval; getter, setter: JSPropertyOp; attrs: uintN; var propp: PJSProperty): JSBool; cdecl;
  JSPropertyIdOp = function(cx: PJSContext; obj: PJSObject; id: jsid; vp: pjsval): JSBool; cdecl;
  JSAttributesOp = function(cx: PJSContext; obj: PJSObject; id: jsid; prop: PJSProperty; attrsp: puintN): JSBool; cdecl;
  JSCheckAccessIdOp = function(cx: PJSContext; obj: PJSObject; id: jsid; mode: JSAccessMode; vp: pjsval; attrsp: puintN): JSBool; cdecl;
  JSObjectOp = function(cx: PJSContext; obj: PJSObject): PJSObject; cdecl;
  JSPropertyRefOp = procedure(cx: PJSContext; obj: PJSObject; prop: PJSProperty); cdecl;
  JSSetObjectSlotOp = function(cx: PJSContext; obj: PJSObject; slot: uint32; pobj: PJSObject): JSBool; cdecl;
  JSGetRequiredSlotOp = function(cx: PJSContext; obj: PJSObject; slot: uint32): jsval; cdecl;
  JSSetRequiredSlotOp = procedure(cx: PJSContext; obj: PJSObject; slot: uint32; v: jsval); cdecl;

  JSNative = function(cx: PJSContext; argc: uintN; vp: pjsval): JSBool; cdecl;
  JSClassInternal = procedure ( ); cdecl;

  JSGCCallback = function(cx: PJSContext; status: JSGCStatus): JSBool; cdecl;
  JSGCRootMapFun = function(rp: Pointer; name: PAnsiChar; data: Pointer): intN; cdecl;
  JSBranchCallback = function(cx: PJSContext; script: PJSObject): JSBool; cdecl;
  JSErrorReporter = procedure(cx: PJSContext; message: PAnsiChar; report: PJSErrorReport); cdecl;
  JSLocaleToUpperCase = function(cx: PJSContext; src: PJSString; rval: pjsval): JSBool; cdecl;
  JSLocaleToLowerCase = function(cx: PJSContext; src: PJSString; rval: pjsval): JSBool; cdecl;
  JSLocaleCompare = function(cx: PJSContext; src1, src2: PJSString; rval: pjsval): JSBool; cdecl;
  JSStringFinalizeOp = procedure(cx: PJSContext; str: PJSString); cdecl;
  JSHashEnumerator = function(he: PJSHashEntry; i: intN; arg: Pointer): intN; cdecl;
  JSHashComparator = function(v1, v2: Pointer): intN; cdecl;
  JSHashFunction = function(key: Pointer): JSHashNumber; cdecl;
  JSPrincipalsTranscoder = function(xdr: PJSXDRState; var principalsp: PJSPrincipals): JSBool; cdecl;

  JSAtomMap = record
    vector: PPJSAtom;
    length: jsatomid;
  end;

  JSScript = record
    code: pjsbytecode;
    length: uint32;
    main: pjsbytecode;
    version: JSVersion;
    atomMap: JSAtomMap;
    filename: PAnsiChar;
    lineno: uintN;
    depth: uintN;
    trynotes: Pointer;
    principals: Pointer;
    _object: Pointer;
  end;

  JSFunction = record
    nrefs: jsrefcount;
    obj: PJSObject;
    native: JSNative;
    script: PJSObject;
    nargs: uint16;
    extra: uint16;
    nvars: uint16;
    flags: uint8;
    spare: uint8;
    atom: PJSAtom;
    clasp: PJSClass;
  end;

  JSFunctionSpec = record
    name: PAnsiChar;
    call: JSNative;
    nargs: uint16;
    flags: uint16;
    extra: uint32;
  end;
//  TJSFunctionsSpecArray = array of JSFunctionSpec;

  JSProperty = record
    id: jsid;
  end;

  JSObjectMap = record
    nrefs: jsrefcount;
    ops: PJSObjectOps;
    nslots: uint32;
    freeslot: uint32;
  end;

  JSObjectOps = record
  (* Mandatory non-null members *)
    newObjectMap: JSNewObjectMapOp;
    destroyObjectMap: JSObjectMapOp;
    lookupProperty: JSLookupPropOp;
    defineProperty: JSDefinePropOp;
    getProperty: JSPropertyIdOp;
    setProperty: JSPropertyIdOp;
    getAttributes: JSAttributesOp;
    setAttributes: JSAttributesOp;
    deleteProperty: JSPropertyIdOp;
    defaultValue: JSConvertOp;
    enumerate: JSNewEnumerateOp;
    checkAccess: JSCheckAccessIdOp;

  (* Optionally non-null members *)
    thisObject: JSObjectOp;
    dropProperty: JSPropertyRefOp;
    call: JSNative;
    construct: JSNative;
    xdrObject: JSXDRObjectOp;
    hasInstance: JSHasInstanceOp;
    setProto: JSSetObjectSlotOp;
    setParent: JSSetObjectSlotOp;
    mark: JSMarkOp;
    clear: JSFinalizeOp;
    getRequiredSlot: JSGetRequiredSlotOp;
    setRequiredSlot: JSSetRequiredSlotOp;
  end;

  JSLocaleCallbacks = record
    localeToUpperCase: JSLocaleToUpperCase;
    localeToLowerCase: JSLocaleToLowerCase;
    localeCompare: JSLocaleCompare;
  end;

  JSExceptionState = record
    throwing: JSBool;
    exception: jsval;
  end;

  JSConstDoubleSpec = record
    dval: jsdouble;
    name: PAnsiChar;
    flags: uint8;
    spare: array[0..2] of uint8;
  end;

  JSPropertySpec = record
    name: PAnsiChar;
    tinyid: int8;
    flags: uint8;
    getter: JSPropertyOp;
    setter: JSStrictPropertyOp;
  end;
  TJSPropertySpecArray = array of JSPropertySpec;

  JSPropertyDesc = record
    id: jsval;
    value: jsval;
    flags: uint8;
    spare: uint8;
    slot: uint16;
    alias: jsval;
  end;

  JSPropertyDescArray = record
    length: uint32;
    _array: PJSPropertyDesc;
  end;

  JSScopeProperty = record
    id: jsid;
    getter: JSPropertyOp;
    setter: JSPropertyOp;
    slot: uint32;
    attrs: uint8;
    flags: uint8;
    shortid: int16;
    parent: PJSScopeProperty;
    kids: PJSScopeProperty;
  end;

  JSClass =  record
    name: PAnsiChar;
    flags: UInt32;

  (* Mandatory non-null function pointer members. *)
    addProperty: JSPropertyOp;
    delProperty: JSPropertyOp;
    getProperty: JSPropertyOp;
    setProperty: JSStrictPropertyOp;
    enumerate: JSEnumerateOp;
    resolve: JSResolveOp;
    convert: JSConvertOp;
    finalize: JSFinalizeOp;

    //* Optionally non-null members start here. */
    reserved0: JSClassInternal;
    checkAccess: JSCheckAccessOp;
    call,
    construct: JSNative;
    xdrObject: JSXDRObjectOp;
    hasInstance: JSHasInstanceOp;
    mark: JSMarkOp;

    reserved1: JSClassInternal;
    reserved: array[0..18] of pointer;
  end;

  JSExtendedClass = record
     base: JSClass;
     equality: JSEqualityOp;
     outerObject: JSObjectOp;
     innerObject: JSObjectOp;
     reserved0: pointer;
     reserved1: pointer;
     reserved2: pointer;
     reserved3: pointer;
     reserved4: pointer;
  end;

  JSErrorReport = record
    filename: PAnsiChar;        // source file name, URL, etc., or null
    lineno: uintN;          // source line number
    linebuf: PAnsiChar;          // offending source line
    tokenptr: PAnsiChar;        // points to error token in linebuf (for caret positioning?)
    uclinebuf: pjschar;      // unicode line buffer
    uctokenptr: pjschar;    // unicode token pointers
    flags: uintN;
    errorNumber: uintN;      // see js.msg
    ucmessage: pjschar;      // default error message
    messageArgs: ppjschar;  // arguments for the error message
  end;

  JSErrorFormatString = record
    format: PAnsiChar;
    argCount: uintN;
  end;

// function  JS_ConvertArguments(cx: PJSContext; argc: uintN; argv: pjsval; format: PAnsiChar; arglist: va_list): JSBool; cdecl; external libName ;
// function  JS_PushArguments(cx: PJSContext; var markp: Pointer; format: PAnsiChar;arglist: va_list: pjsval; cdecl; external libName ;
// function  JS_ReportErrorFlagsAndNumber(cx: PJSContext; flags: uintN; errorCallback: JSErrorCallback; userRef: Pointer; errorNumber: const uintN; arglist: va_list): JSBool; cdecl; external libName ;
// function  JS_ReportErrorFlagsAndNumberUC(cx: PJSContext; flags: uintN; errorCallback: JSErrorCallback; userRef: Pointer; errorNumber: const uintN; arglist: va_list): JSBool; cdecl; external libName ;
function  JS_ReportWarning(cx: PJSContext; format: PAnsiChar; arglist: va_list): JSBool; cdecl; external libName ;
// procedure JS_ReportError(cx: PJSContext; format: PAnsiChar; : ...); cdecl; external libName ;
// procedure JS_ReportErrorNumber(cx: PJSContext; errorCallback: JSErrorCallback; userRef: Pointer; errorNumber: const uintN; : ...); cdecl; external libName ;
// procedure JS_ReportErrorNumberUC(cx: PJSContext; errorCallback: JSErrorCallback; userRef: Pointer; errorNumber: const uintN; : ...); cdecl; external libName ;
// function wvsprintf(Output: PAnsiChar; Format: PAnsiChar; arglist: va_list): Integer; stdcall;
procedure JS_ReportError(cx: PJSContext; format: PAnsiChar; arglist: va_list); cdecl; external libName ;

function JS_ComputeThis(cx: PJSContext; vp: pjsval):jsval; cdecl; external libName ;
function JS_AddExternalStringFinalizer(finalizer: JSStringFinalizeOp): intN; cdecl; external libName ;

function JS_AddValueRoot(cx: PJSContext; vp: pjsval): JSBool; cdecl; external libName ;
function JS_AddStringRoot(cx: PJSContext; vp: PPJSString): JSBool; cdecl; external libName ;
function JS_AddObjectRoot(cx: PJSContext; vp: PPJSObject): JSBool; cdecl; external libName ;
function JS_AddGCThingRoot(cx: PJSContext; vp: PPointer): JSBool; cdecl; external libName ;

function JS_AddNamedValueRoot(cx: PJSContext; vp: pjsval; name: PAnsiChar): JSBool; cdecl; external libName ;
function JS_AddNamedStringRoot(cx: PJSContext; vp: PPJSString; name: PAnsiChar): JSBool; cdecl; external libName ;
function JS_AddNamedObjectRoot(cx: PJSContext; vp: PPJSObject; name: PAnsiChar): JSBool; cdecl; external libName ;
function JS_AddNamedGCThingRoot(cx: PJSContext; vp: PPointer; name: PAnsiChar): JSBool; cdecl; external libName ;

function JS_RemoveValueRoot(cx: PJSContext; vp: pjsval): JSBool; cdecl; external libName ;
function JS_RemoveStringRoot(cx: PJSContext; vp: PPJSString): JSBool; cdecl; external libName ;
function JS_RemoveObjectRoot(cx: PJSContext; vp: PPJSObject): JSBool; cdecl; external libName ;
function JS_RemoveGCThingRoot(cx: PJSContext; vp: PPointer): JSBool; cdecl; external libName ;


//function JS_AddNamedRoot(cx: PJSContext; rp: Pointer; name: PAnsiChar): JSBool; cdecl; external libName ;
//function JS_AddNamedRootRT(rt: PJSRuntime; rp: Pointer; name: PAnsiChar): JSBool; cdecl; external libName ;
//function JS_AddRoot(cx: PJSContext; rp: Pointer): JSBool; cdecl; external libName ;


procedure JS_ForgetLocalRoot(cx: PJSContext; thing: pointer); cdecl; external libName ;
function JS_EnterLocalRootScope(cx: PJSContext): JSBool; cdecl; external libName ;
procedure JS_LeaveLocalRootScope(cx: PJSContext); cdecl; external libName ;

function JS_AliasElement(cx: PJSContext; obj: PJSObject; name: PAnsiChar; alias: jsint): JSBool; cdecl; external libName ;
function JS_AliasProperty(cx: PJSContext; obj: PJSObject; name: PAnsiChar; alias: PAnsiChar): JSBool; cdecl; external libName ;
function JS_BufferIsCompilableUnit(cx: PJSContext; obj: PJSObject; bytes: PAnsiChar; length: size_t): JSBool; cdecl; external libName ;
function JS_CallFunction(cx: PJSContext; obj: PJSObject; fun: PJSFunction; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl; external libName ;
function JS_CallFunctionName(cx: PJSContext; obj: PJSObject; name: PAnsiChar; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl; external libName ;
function JS_CallFunctionValue(cx: PJSContext; obj: PJSObject; fval: jsval; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl; external libName ;
//--function JS_CallUCFunctionName(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl; external libName ;
function JS_CheckAccess(cx: PJSContext; obj: PJSObject; id: jsid; mode: JSAccessMode; vp: pjsval; attrsp: puintN): JSBool; cdecl; external libName ;
function JS_CloneFunctionObject(cx: PJSContext; funobj: PJSObject; parent: PJSObject): PJSObject; cdecl; external libName ;
function JS_CompareStrings(str1: PJSString; str2: PJSString): intN; cdecl; external libName ;

function JS_ConcatStrings(cx: PJSContext; left: PJSString; right: PJSString): PJSString; cdecl; external libName ;
function JS_ConstructObject(cx: PJSContext; clasp: PJSClass; proto: PJSObject; parent: PJSObject): PJSObject; cdecl; external libName ;
function JS_ConstructObjectWithArguments(cx: PJSContext; clasp: PJSClass; proto: PJSObject; parent: PJSObject; argc: uintN; argv: pjsval): PJSObject; cdecl; external libName ;
function JS_ContextIterator(rt: PJSRuntime; var iterp: PJSContext): PJSContext; cdecl; external libName ;
function JS_ConvertStub(cx: PJSContext; obj: PJSObject; _type: JSType; vp: pjsval): JSBool; cdecl; external libName ;
function JS_ConvertValue(cx: PJSContext; v: jsval; _type: JSType; vp: pjsval): JSBool; cdecl; external libName ;
function JS_DecompileFunction(cx: PJSContext; fun: PJSFunction; indent: uintN): PJSString; cdecl; external libName ;
function JS_DecompileFunctionBody(cx: PJSContext; fun: PJSFunction; indent: uintN): PJSString; cdecl; external libName ;
function JS_DecompileScript(cx: PJSContext; script: PJSObject; name: PAnsiChar; indent: uintN): PJSString; cdecl; external libName ;
function JS_DefineConstDoubles(cx: PJSContext; obj: PJSObject; cds: PJSConstDoubleSpec): JSBool; cdecl; external libName ;
function JS_DefineElement(cx: PJSContext; obj: PJSObject; index: jsint; value: jsval; getter: JSPropertyOp; setter: JSPropertyOp; attrs: uintN): JSBool; cdecl; external libName ;
function JS_DefineFunction(cx: PJSContext; obj: PJSObject; name: PAnsiChar; call: JSNative; nargs: uintN; attrs: uintN): PJSFunction; cdecl; external libName ;
function JS_DefineFunctions(cx: PJSContext; obj: PJSObject; fs: PJSFunctionSpec): JSBool; cdecl; external libName ;
function JS_DefineObject(cx: PJSContext; obj: PJSObject; name: PAnsiChar; clasp: PJSClass; proto: PJSObject; attrs: uintN): PJSObject; cdecl; external libName ;
function JS_DefineProperties(cx: PJSContext; obj: PJSObject; ps: PJSPropertySpec): JSBool; cdecl; external libName ;
function JS_DefineProperty(cx: PJSContext; obj: PJSObject; name: PAnsiChar; value: jsval; getter: JSPropertyOp; setter: JSStrictPropertyOp ; attrs: uintN): JSBool; cdecl; external libName ;
function JS_DefinePropertyWithTinyId(cx: PJSContext; obj: PJSObject; name: PAnsiChar; tinyid: int8; value: jsval; getter: JSPropertyOp; setter: JSStrictPropertyOp ; attrs: uintN): JSBool; cdecl; external libName ;
function JS_DefineUCFunction(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; call: JSNative; nargs: uintN; attrs: uintN): PJSFunction; cdecl; external libName ;
function JS_DefineUCProperty(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; value: jsval; getter: JSPropertyOp; setter: JSPropertyOp; attrs: uintN): JSBool; cdecl; external libName ;
function JS_DefineUCPropertyWithTinyId(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; tinyid: int8; value: jsval; getter: JSPropertyOp; setter: JSPropertyOp; attrs: uintN): JSBool; cdecl; external libName ;
function JS_DeleteElement(cx: PJSContext; obj: PJSObject; index: jsint): JSBool; cdecl; external libName ;
function JS_DeleteElement2(cx: PJSContext; obj: PJSObject; index: jsint; rval: pjsval): JSBool; cdecl; external libName ;
function JS_DeleteProperty(cx: PJSContext; obj: PJSObject; name: PAnsiChar): JSBool; cdecl; external libName ;
function JS_DeleteProperty2(cx: PJSContext; obj: PJSObject; name: PAnsiChar; rval: pjsval): JSBool; cdecl; external libName ;
function JS_DeleteUCProperty2(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; rval: pjsval): JSBool; cdecl; external libName ;
function JS_Enumerate(cx: PJSContext; obj: PJSObject): PJSIdArray; cdecl; external libName ;
function JS_EnumerateStandardClasses(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external libName ;
function JS_EnumerateStub(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external libName ;
function JS_ErrorFromException(cx: PJSContext; v: jsval): PJSErrorReport; cdecl; external libName ;
function JS_EvaluateScript(cx: PJSContext; obj: PJSObject; bytes: PAnsiChar; length: uintN; filename: PAnsiChar; lineno: uintN; rval: pjsval): JSBool; cdecl; external libName ;
function JS_EvaluateScriptForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; bytes: PAnsiChar; length: uintN; filename: PAnsiChar; lineno: uintN; rval: pjsval): JSBool; cdecl; external libName ;
function JS_EvaluateUCScript(cx: PJSContext; obj: PJSObject; chars: pjschar; length: uintN; filename: PAnsiChar; lineno: uintN; rval: pjsval): JSBool; cdecl; external libName ;
function JS_EvaluateUCScriptForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; chars: pjschar; length: uintN; filename: PAnsiChar; lineno: uintN; rval: pjsval): JSBool; cdecl; external libName ;
function JS_ExecuteScript(cx: PJSContext; obj: PJSObject; script: PJSObject; rval: pjsval): JSBool; cdecl; external libName ;
function JS_ExecuteScriptPart(cx: PJSContext; obj: PJSObject; script: PJSObject; part: JSExecPart; rval: pjsval): JSBool; cdecl; external libName ;
function JS_GetArrayLength(cx: PJSContext; obj: PJSObject; var length: jsuint): JSBool; cdecl; external libName ;
function JS_GetClass(obj: PJSObject): PJSClass; cdecl; external libName ;
function JS_GetConstructor(cx: PJSContext; proto: PJSObject): PJSObject; cdecl; external libName ;
function JS_GetContextPrivate(cx: PJSContext): Pointer; cdecl; external libName ;
function JS_GetElement(cx: PJSContext; obj: PJSObject; index: jsint; vp: pjsval): JSBool; cdecl; external libName ;
function JS_GetEmptyStringValue(cx: PJSContext): jsval; cdecl; external libName ;
function JS_GetExternalStringGCType(rt: PJSRuntime; str: PJSString): intN; cdecl; external libName ;
function JS_GetFunctionFlags(fun: PJSFunction): uintN; cdecl; external libName ;
function JS_GetFunctionId(fun: PJSFunction): PJSString; cdecl; external libName ;
//function JS_GetFunctionName(fun: PJSFunction): PAnsiChar; cdecl; external libName ;
function JS_GetFunctionObject(fun: PJSFunction): PJSObject; cdecl; external libName ;
function JS_GetGlobalObject(cx: PJSContext): PJSObject; cdecl; external libName ;
function JS_GetImplementationVersion: PAnsiChar; cdecl; external libName ;
function JS_GetInstancePrivate(cx: PJSContext; obj: PJSObject; clasp: PJSClass; argv: pjsval): Pointer; cdecl; external libName ;
function JS_GetLocaleCallbacks(cx: PJSContext): PJSLocaleCallbacks; cdecl; external libName ;
function JS_GetNaNValue(cx: PJSContext): jsval; cdecl; external libName ;
function JS_GetNegativeInfinityValue(cx: PJSContext): jsval; cdecl; external libName ;
function JS_GetOptions(cx: PJSContext): uint32; cdecl; external libName ;
function JS_GetParent(cx: PJSContext; obj: PJSObject): PJSObject; cdecl; external libName ;
function JS_GetPendingException(cx: PJSContext; vp: pjsval): JSBool; cdecl; external libName ;
function JS_GetPositiveInfinityValue(cx: PJSContext): jsval; cdecl; external libName ;
function JS_GetPrivate(cx: PJSContext; obj: PJSObject): Pointer; cdecl; external libName ;

function JS_GetProperty(cx: PJSContext; obj: PJSObject; name: PAnsiChar; vp: pjsval): JSBool; cdecl; external libName ;
function JS_GetPropertyAttributes(cx: PJSContext; obj: PJSObject; name: PAnsiChar; attrsp: puintN; foundp: PJSBool): JSBool; cdecl; external libName ;



function JS_GetPrototype(cx: PJSContext; obj: PJSObject): PJSObject; cdecl; external libName ;
function JS_GetReservedSlot(cx: PJSContext; obj: PJSObject; index: uint32; vp: pjsval): JSBool; cdecl; external libName ;
function JS_GetRuntime(cx: PJSContext): PJSRuntime; cdecl; external libName ;
function JS_GetScopeChain(cx: PJSContext): PJSObject; cdecl; external libName ;
function JS_GetScriptObject(script: PJSObject): PJSObject; cdecl; external libName ;
function JS_GetStringBytes(str: PJSString): PAnsiChar; cdecl; external libName ;
function JS_GetStringCharsZ(cx: PJSContext; str: PJSString): pjschar; cdecl; external libName ;
function JS_GetStringLength(str: PJSString): size_t; cdecl; external libName ;
function JS_GetStringCharsZAndLength(cx: PJSContext; str: PJSString; s: size_t): pjschar; cdecl; external libName ;

function JS_GetTypeName(cx: PJSContext; _type: JSType): PAnsiChar; cdecl; external libName ;
function JS_GetUCProperty(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; vp: pjsval): JSBool; cdecl; external libName ;
function JS_GetUCPropertyAttributes(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; attrsp: puintN; foundp: PJSBool): JSBool; cdecl; external libName ;
function JS_GetVersion(cx: PJSContext): JSVersion; cdecl; external libName ;
function JS_HasArrayLength(cx: PJSContext; obj: PJSObject; var length: jsuint): JSBool; cdecl; external libName ;
function JS_IdToValue(cx: PJSContext; id: jsid; vp: pjsval): JSBool; cdecl; external libName ;
function JS_Init(maxbytes: uint32): PJSRuntime; cdecl; external libName ;
function JS_GetRuntimePrivate(rt: PJSRuntime): pointer;cdecl; external libName ;
procedure JS_SetRuntimePrivate(rt: PJSRuntime; data: pointer);cdecl; external libName ;

function JS_NewRuntime(maxbytes: uint32): PJSRuntime; cdecl; external LibName name 'JS_Init' ;
procedure JS_DestroyRuntime(rt: PJSRuntime); cdecl; external LibName name 'JS_Finish' ;

function JS_NewGlobalObject(cx: PJSContext; clasp: pJSClass): PJSObject; cdecl; external libName ;
function JS_NewCompartmentAndGlobalObject(cx: PJSContext; clasp: pJSClass; principals: pJSPrincipals): PJSObject; cdecl; external libName ;

function JS_NewObjectForConstructor(cx: PJSContext; const vp: Pjsval): PJSObject; cdecl; external libName ;

function JS_InitClass(cx: PJSContext; obj: PJSObject; parent_proto: PJSObject;clasp: PJSClass; _constructor: JSNative; nargs: uintN; ps: PJSPropertySpec; fs: PJSFunctionSpec;static_ps: PJSPropertySpec; static_fs: PJSFunctionSpec): PJSObject; cdecl; external libName ;
function JS_InitStandardClasses(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external libName ;
function JS_InstanceOf(cx: PJSContext; obj: PJSObject; clasp: PJSClass; argv: pjsval): JSBool; cdecl; external libName ;
function JS_InternString(cx: PJSContext; s: PAnsiChar): PJSString; cdecl; external libName ;
function JS_InternUCString(cx: PJSContext; s: pjschar): PJSString; cdecl; external libName ;
function JS_InternUCStringN(cx: PJSContext; s: pjschar; length: size_t): PJSString; cdecl; external libName ;
function JS_IsAboutToBeFinalized(cx: PJSContext; thing: Pointer): JSBool; cdecl; external libName ;
function JS_IsArrayObject(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external libName ;

function JS_IsConstructing(cx: PJSContext; vp: pjsval): boolean;
function JS_IsExceptionPending(cx: PJSContext): JSBool; cdecl; external libName ;
function JS_IsRunning(cx: PJSContext): JSBool; cdecl; external libName ;
function JS_LockGCThing(cx: PJSContext; thing: Pointer): JSBool; cdecl; external libName ;
function JS_LockGCThingRT(rt: PJSRuntime; thing: Pointer): JSBool; cdecl; external libName ;
function JS_LookupElement(cx: PJSContext; obj: PJSObject; index: jsint; vp: pjsval): JSBool; cdecl; external libName ;
function JS_LookupProperty(cx: PJSContext; obj: PJSObject; name: PAnsiChar; vp: pjsval): JSBool; cdecl; external libName ;
function JS_LookupUCProperty(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; vp: pjsval): JSBool; cdecl; external libName ;
function JS_MakeStringImmutable(cx: PJSContext; str: PJSString): JSBool; cdecl; external libName ;
function JS_malloc(cx: PJSContext; nbytes: size_t): Pointer; cdecl; external libName ;
function JS_MapGCRoots(rt: PJSRuntime; map: JSGCRootMapFun; data: Pointer): uint32; cdecl; external libName ;
function JS_NewArrayObject(cx: PJSContext; length: jsint; vector: pjsval): PJSObject; cdecl; external libName ;
function JS_NewContext(rt: PJSRuntime; stackChunkSize: size_t): PJSContext; cdecl; external libName ;
function JS_NewDependentString(cx: PJSContext; str: PJSString; start: size_t; length: size_t): PJSString; cdecl; external libName ;
function JS_NewDouble(cx: PJSContext; d: jsdouble): pjsdouble; cdecl; external libName ;
function JS_NewDoubleValue(cx: PJSContext; d: jsdouble; rval: pjsval): JSBool; cdecl; external libName ;
function JS_NewExternalString(cx: PJSContext; chars: pjschar; length: size_t; _type: intN): PJSString; cdecl; external libName ;
function JS_NewFunction(cx: PJSContext; call: JSNative; nargs: uintN; flags: uintN; parent: PJSObject; name: PAnsiChar): PJSFunction; cdecl; external libName ;
function JS_NewGrowableString(cx: PJSContext; chars: pjschar; length: size_t): PJSString; cdecl; external libName ;
function JS_NewNumberValue(cx: PJSContext; d: jsdouble; rval: pjsval): JSBool; cdecl; external libName ;
function JS_NewObject(cx: PJSContext; clasp: PJSClass; proto: PJSObject; parent: PJSObject): PJSObject; cdecl; external libName ;
function JS_NewRegExpObject(cx: PJSContext; bytes: PAnsiChar; length: size_t; flags: uintN): PJSObject; cdecl; external libName ;
function JS_NewString(cx: PJSContext; bytes: PAnsiChar; length: size_t): PJSString; cdecl; external libName ;
function JS_NewStringCopyN(cx: PJSContext; s: PAnsiChar; n: size_t): PJSString; cdecl; external libName ;
function JS_NewStringCopyZ(cx: PJSContext; s: PAnsiChar): PJSString; cdecl; external libName ;
function JS_NewUCRegExpObject(cx: PJSContext; chars: pjschar; length: size_t; flags: uintN): PJSObject; cdecl; external libName ;
function JS_NewUCString(cx: PJSContext; chars: pjschar; length: size_t): PJSString; cdecl; external libName ;
function JS_NewUCStringCopyN(cx: PJSContext; s: pjschar; n: size_t): PJSString; cdecl; external libName ;
function JS_NewUCStringCopyZ(cx: PJSContext; s: pjschar): PJSString; cdecl; external libName ;
function JS_Now: int64; cdecl; external libName ;
function JS_ObjectIsFunction(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external libName ;
function JS_PropertyStub(cx: PJSContext; obj: PJSObject; id: jsid; vp: pjsval): JSBool; cdecl; external libName ;
function JS_StrictPropertyStub(cx: PJSContext; obj: PJSObject; id: jsid; _strict:jsbool; vp: pjsval): JSBool; cdecl; external libName ;
function JS_realloc(cx: PJSContext; p: Pointer; nbytes: size_t): Pointer; cdecl; external libName ;

function JS_RemoveExternalStringFinalizer(finalizer: JSStringFinalizeOp): intN; cdecl; external libName ;

function JS_ResolveStandardClass(cx: PJSContext; obj: PJSObject; id: jsval; resolved: PJSBool): JSBool; cdecl; external libName ;
function JS_ResolveStub(cx: PJSContext; obj: PJSObject; id: jsid): JSBool; cdecl; external libName ;
function JS_SaveExceptionState(cx: PJSContext): PJSExceptionState; cdecl; external libName ;
function JS_SetArrayLength(cx: PJSContext; obj: PJSObject; length: jsuint): JSBool; cdecl; external libName ;
function JS_SetBranchCallback(cx: PJSContext; cb: JSBranchCallback): JSBranchCallback; cdecl; external libName ;
function JS_SetCheckObjectAccessCallback(rt: PJSRuntime; acb: JSCheckAccessOp): JSCheckAccessOp; cdecl; external libName ;
function JS_SetElement(cx: PJSContext; obj: PJSObject; index: jsint; vp: pjsval): JSBool; cdecl; external libName ;
function JS_SetErrorReporter(cx: PJSContext; er: JSErrorReporter): JSErrorReporter; cdecl; external libName ;
function JS_SetGCCallback(cx: PJSContext; cb: JSGCCallback): JSGCCallback; cdecl; external libName ;
function JS_SetGCCallbackRT(rt: PJSRuntime; cb: JSGCCallback): JSGCCallback; cdecl; external libName ;
function JS_SetOptions(cx: PJSContext; options: uint32): uint32; cdecl; external libName ;
function JS_SetParent(cx: PJSContext; obj: PJSObject; parent: PJSObject): JSBool; cdecl; external libName ;
function JS_SetPrincipalsTranscoder(rt: PJSRuntime; px: JSPrincipalsTranscoder): JSPrincipalsTranscoder; cdecl; external libName ;
function JS_SetPrivate(cx: PJSContext; obj: PJSObject; data: Pointer): JSBool; cdecl; external libName ;
function JS_SetProperty(cx: PJSContext; obj: PJSObject; name: PAnsiChar; vp: pjsval): JSBool; cdecl; external libName ;
function JS_SetPropertyAttributes(cx: PJSContext; obj: PJSObject; name: PAnsiChar; attrs: uintN; foundp: PJSBool): JSBool; cdecl; external libName ;
function JS_SetPrototype(cx: PJSContext; obj: PJSObject; proto: PJSObject): JSBool; cdecl; external libName ;
function JS_SetReservedSlot(cx: PJSContext; obj: PJSObject; index: uint32; v: jsval): JSBool; cdecl; external libName ;
function JS_SetUCProperty(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; vp: pjsval): JSBool; cdecl; external libName ;
function JS_SetUCPropertyAttributes(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; attrs: uintN; foundp: PJSBool): JSBool; cdecl; external libName ;
function JS_SetVersion(cx: PJSContext; version: JSVersion): JSVersion; cdecl; external libName ;
function JS_strdup(cx: PJSContext; s: PAnsiChar): PAnsiChar; cdecl; external libName ;
function JS_StringToVersion(_string: PAnsiChar): JSVersion; cdecl; external libName ;
function JS_ToggleOptions(cx: PJSContext; options: uint32): uint32; cdecl; external libName ;
function JS_TypeOfValue(cx: PJSContext; v: jsval): JSType; cdecl; external libName ;
//function JS_UCBufferIsCompilableUnit(cx: PJSContext; obj: PJSObject; chars: pjschar; length: size_t): JSBool; cdecl; external libName ;
function JS_UndependString(cx: PJSContext; str: PJSString): pjschar; cdecl; external libName ;
function JS_UnlockGCThing(cx: PJSContext; thing: Pointer): JSBool; cdecl; external libName ;
function JS_UnlockGCThingRT(rt: PJSRuntime; thing: Pointer): JSBool; cdecl; external libName ;
function JS_ValueToBoolean(cx: PJSContext; v: jsval; bp: PJSBool): JSBool; cdecl; external libName ;
function JS_ValueToConstructor(cx: PJSContext; v: jsval): PJSFunction; cdecl; external libName ;
function JS_ValueToFunction(cx: PJSContext; v: jsval): PJSFunction; cdecl; external libName ;
function JS_ValueToId(cx: PJSContext; v: jsval; idp: pjsid): JSBool; cdecl; external libName ;

function JS_ValueToECMAInt32(cx: PJSContext; v: jsval; ip: pint32): JSBool; cdecl; external libName ;
function JS_ValueToECMAUint32(cx: PJSContext; v: jsval; ip: puint32): JSBool; cdecl; external libName ;

function JS_ValueToInt32(cx: PJSContext; v: jsval; ip: pint32): JSBool; cdecl; external libName ;
function JS_ValueToUint16(cx: PJSContext; v: jsval; ip: puint16): JSBool; cdecl; external libName ;

function JS_ValueToNumber(cx: PJSContext; v: jsval; dp: pjsdouble): JSBool; cdecl; external libName ;
function JS_ValueToObject(cx: PJSContext; v: jsval; var objp: PJSObject): JSBool; cdecl; external libName ;
function JS_ValueToString(cx: PJSContext; v: jsval): PJSString; cdecl; external libName ;
function JS_VersionToString(version: JSVersion): PAnsiChar; cdecl; external libName ;

function JS_XDRBytes(xdr: PJSXDRState; bytes: PAnsiChar; len: uint32): JSBool; cdecl; external libName ;
function JS_XDRCString(xdr: PJSXDRState; var s: PAnsiChar): JSBool; cdecl; external libName ;
function JS_XDRCStringOrNull(xdr: PJSXDRState; var s: PAnsiChar): JSBool; cdecl; external libName ;
function JS_XDRDouble(xdr: PJSXDRState; var d: pjsdouble): JSBool; cdecl; external libName ;
function JS_XDRFindClassById(xdr: PJSXDRState; id: uint32): PJSClass; cdecl; external libName ;
function JS_XDRFindClassIdByName(xdr: PJSXDRState; name: PAnsiChar): JSBool; cdecl; external libName ;
function JS_XDRMemDataLeft(xdr: PJSXDRState): uint32; cdecl; external libName ;
function JS_XDRMemGetData(xdr: PJSXDRState; lp: puint32): Pointer; cdecl; external libName ;
function JS_XDRNewMem(cx: PJSContext; mode: JSXDRMode): PJSXDRState; cdecl; external libName ;
function JS_XDRRegisterClass(xdr: PJSXDRState; clasp: PJSClass; lp: uint32): JSBool; cdecl; external libName ;

function JS_XDRScript(xdr: PJSXDRState; var script: PJSObject): JSBool; cdecl; external libName name 'JS_XDRScriptObject';

function JS_XDRString(xdr: PJSXDRState; var str: PJSString): JSBool; cdecl; external libName ;
function JS_XDRStringOrNull(xdr: PJSXDRState; var str: PJSString): JSBool; cdecl; external libName ;
function JS_XDRUint16(xdr: PJSXDRState; s: puint16): JSBool; cdecl; external libName ;
function JS_XDRUint32(xdr: PJSXDRState; lp: puint32): JSBool; cdecl; external libName ;
function JS_XDRUint8(xdr: PJSXDRState; b: puint8): JSBool; cdecl; external libName ;
function JS_XDRValue(xdr: PJSXDRState; vp: pjsval): JSBool; cdecl; external libName ;
procedure JS_XDRDestroy(xdr: PJSXDRState); cdecl; external libName ;
procedure JS_XDRInitBase(xdr: PJSXDRState; mode: JSXDRMode; cx: PJSContext); cdecl; external libName ;
procedure JS_XDRMemResetData(xdr: PJSXDRState); cdecl; external libName ;
procedure JS_XDRMemSetData(xdr: PJSXDRState; data: Pointer; len: uint32); cdecl; external libName ;


procedure JS_ClearNewbornRoots(cx: PJSContext); cdecl; external libName ;
procedure JS_ClearPendingException(cx: PJSContext); cdecl; external libName ;
procedure JS_ClearRegExpRoots(cx: PJSContext); cdecl; external libName ;
procedure JS_ClearRegExpStatics(cx: PJSContext); cdecl; external libName ;
procedure JS_ClearScope(cx: PJSContext; obj: PJSObject); cdecl; external libName ;
procedure JS_DestroyContext(cx: PJSContext); cdecl; external libName ;
procedure JS_DestroyContextMaybeGC(cx: PJSContext); cdecl; external libName ;
procedure JS_DestroyContextNoGC(cx: PJSContext); cdecl; external libName ;
procedure JS_DestroyIdArray(cx: PJSContext; ida: PJSIdArray); cdecl; external libName ;
procedure JS_DropExceptionState(cx: PJSContext; state: PJSExceptionState); cdecl; external libName ;
procedure JS_FinalizeStub(cx: PJSContext; obj: PJSObject); cdecl; external libName ;
procedure JS_Finish(rt: PJSRuntime); cdecl; external libName ;
procedure JS_free(cx: PJSContext; p: Pointer); cdecl; external libName ;
procedure JS_GC(cx: PJSContext); cdecl; external libName ;
procedure JS_Lock(rt: PJSRuntime); cdecl; external libName ;
procedure JS_MarkGCThing(cx: PJSContext; thing: Pointer; name: PAnsiChar; arg: Pointer); cdecl; external libName ;
procedure JS_MaybeGC(cx: PJSContext); cdecl; external libName ;
procedure JS_PopArguments(cx: PJSContext; mark: Pointer); cdecl; external libName ;
procedure JS_ReportOutOfMemory(cx: PJSContext); cdecl; external libName ;
procedure JS_RestoreExceptionState(cx: PJSContext; state: PJSExceptionState); cdecl; external libName ;
procedure JS_SetCallReturnValue2(cx: PJSContext; v: jsval); cdecl; external libName ;
procedure JS_SetContextPrivate(cx: PJSContext; data: Pointer); cdecl; external libName ;
procedure JS_SetGlobalObject(cx: PJSContext; obj: PJSObject); cdecl; external libName ;
procedure JS_SetLocaleCallbacks(cx: PJSContext; callbacks: PJSLocaleCallbacks); cdecl; external libName ;
procedure JS_SetPendingException(cx: PJSContext; v: jsval); cdecl; external libName ;
procedure JS_SetRegExpInput(cx: PJSContext; input: PJSString; multiline: JSBool); cdecl; external libName ;
procedure JS_ShutDown; cdecl; external libName ;
procedure JS_Unlock(rt: PJSRuntime); cdecl; external libName ;



function JS_NewPropertyIterator(cx: PJSContext; obj: PJSObject): PJSObject; cdecl; external libName ;
function JS_NextProperty(cx: PJSContext; iterobj: PJSObject;  var idp: jsid): JSBOOL; cdecl; external libName ;
function JS_GetMethodById(cx: PJSContext; obj: PJSObject;id: jsid ;var objp: PJSObject; vp: Pjsval): JSBOOL; cdecl; external libName ;

(* Debug API *)
//     JS_AliasElement : function (cx: PJSContext; obj: PJSObject; name: PAnsiChar; alias: jsint): JSBool; cdecl;
//JSOp = integer;
function JS_SetTrap(cx: PJSContext; script: PJSObject; pc: pjsbytecode; handler: JSTrapHandler; closure: pointer): JSBOOL; cdecl; external libName ;
function JS_GetTrapOpcode(cx: PJSContext; script: PJSObject;pc: Pjsbytecode): JSOp{ integer}; cdecl; external libName ;
procedure JS_ClearTrap(cx: PJSContext; script: PJSObject; pc: Pjsbytecode; var handlerp: JSTrapHandler; var closurep: pointer); cdecl; external libName ;
procedure JS_ClearScriptTraps(cx: PJSContext; script: PJSObject); cdecl; external libName ;
procedure JS_ClearAllTraps(cx: PJSContext); cdecl; external libName ;
function JS_HandleTrap(cx: PJSContext;script: PJSObject;  pc: Pjsbytecode; rval: Pjsval): JSTrapStatus; cdecl; external libName ;
function JS_SetInterrupt(rt: PJSRuntime;  handler: JSTrapHandler; closure: pointer): JSBool; cdecl; external libName ;
function JS_ClearInterrupt(rt: PJSRuntime; var handlerp: JSTrapHandler; var closurep: pointer): JSBool; cdecl; external libName ;
function JS_SetWatchPoint(cx: PJSContext; obj: PJSObject; id: jsval;
                  handler: JSWatchPointHandler; closure: pointer): JSBool; cdecl; external libName ;

function JS_ClearWatchPoint(cx: PJSContext; obj: PJSObject;  id: jsval;
                   var handlerp: JSWatchPointHandler; var closurep: pointer): JSBool; cdecl; external libName ;
function JS_ClearWatchPointsForObject(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external libName ;
function JS_ClearAllWatchPoints(cx: PJSContext): JSBool; cdecl; external libName ;
function JS_PCToLineNumber(cx: PJSContext;  script: PJSObject; pc: pjsbytecode): uintN; cdecl; external libName ;
function JS_LineNumberToPC(cx: PJSContext;  script: PJSObject; lineno: uintN ): pjsbytecode; cdecl; external libName ;
function JS_GetFunctionScript(cx: PJSContext;  fun: PJSFunction): PJSObject; cdecl; external libName ;
function JS_GetFunctionNative(cx: PJSContext;  fun: PJSFunction): JSNative; cdecl; external libName ;
function JS_GetScriptPrincipals(cx: PJSContext;  script: PJSObject): PJSPrincipals; cdecl; external libName ;

function JS_FrameIterator(cx: PJSContext; var iteratorp: PJSStackFrame): PJSStackFrame;cdecl; external libName ;
function JS_GetFrameScript(cx: PJSContext; fp: PJSStackFrame): PJSObject;cdecl; external libName ;

function JS_GetFramePC(cx: PJSContext; fp: PJSStackFrame): pjsbytecode;cdecl; external libName ;
function JS_GetScriptedCaller(cx: PJSContext; fp: PJSStackFrame): PJSStackFrame;cdecl; external libName ;

function JS_StackFramePrincipals(cx: PJSContext;fp: PJSStackFrame): PJSPrincipals;cdecl; external libName ;
function JS_EvalFramePrincipals(cx: PJSContext; fp: PJSStackFrame; caller: PJSStackFrame): PJSPrincipals;cdecl; external libName ;
function JS_GetFrameAnnotation(cx: PJSContext; fp: PJSStackFrame): pointer;cdecl; external libName ;
procedure JS_SetFrameAnnotation(cx: PJSContext; fp: PJSStackFrame; annotation: pointer);cdecl; external libName ;
function JS_GetFramePrincipalArray(cx: PJSContext;  fp: PJSStackFrame): pointer;cdecl; external libName ;

//function JS_IsScriptFrame(cx: PJSContext; fp: pJSStackFrame): JSBool;cdecl; external libName ;
function JS_IsNativeFrame(cx: PJSContext; fp: pJSStackFrame): JSBool;cdecl; external libName name 'JS_IsScriptFrame';

function JS_CompileFile(cx: PJSContext; obj: PJSObject; filename: PAnsiChar): PJSObject; cdecl; external libName ;
function JS_CompileFileHandle(cx: PJSContext; obj: PJSObject; filename: PAnsiChar; fh: PFILE): PJSObject; cdecl; external libName ;
function JS_CompileFileHandleForPrincipals(cx: PJSContext; obj: PJSObject; filename: PAnsiChar; fh: PFILE; principals: PJSPrincipals): PJSObject; cdecl; external libName ;
function JS_CompileFunction(cx: PJSContext; obj: PJSObject; name: PAnsiChar; nargs: uintN; var argnames: PAnsiChar; bytes: PAnsiChar; length: size_t; filename: PAnsiChar; lineno: uintN): PJSFunction; cdecl; external libName ;
function JS_CompileFunctionForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; name: PAnsiChar; nargs: uintN; var argnames: PAnsiChar; bytes: PAnsiChar; length: size_t; filename: PAnsiChar; lineno: uintN): PJSFunction; cdecl; external libName ;
function JS_CompileScript(cx: PJSContext; obj: PJSObject; bytes: PAnsiChar; length: size_t; filename: PAnsiChar; lineno: uintN): PJSObject; cdecl; external libName ;
function JS_CompileScriptForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; bytes: PAnsiChar; length: size_t; filename: PAnsiChar; lineno: uintN): PJSObject; cdecl; external libName ;
function JS_CompileUCFunction(cx: PJSContext; obj: PJSObject; name: PAnsiChar; nargs: uintN; var argnames: PAnsiChar; chars: pjschar; length: size_t; filename: PAnsiChar; lineno: uintN): PJSFunction; cdecl; external libName ;
function JS_CompileUCFunctionForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; name: PAnsiChar; nargs: uintN; var argnames: PAnsiChar; chars: pjschar; length: size_t; filename: PAnsiChar; lineno: uintN): PJSFunction; cdecl; external libName ;
function JS_CompileUCScript(cx: PJSContext; obj: PJSObject; chars: pjschar; length: size_t; filename: PAnsiChar; lineno: uintN): PJSObject; cdecl; external libName ;
function JS_CompileUCScriptForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; chars: pjschar; length: size_t; filename: PAnsiChar; lineno: uintN): PJSObject; cdecl; external libName ;

function JS_GetFrameScopeChain(cx: PJSContext; fp: pJSStackFrame): pJSObject;cdecl; external libName ;
function JS_GetFrameCallObject(cx: PJSContext; fp: pJSStackFrame): pJSObject;cdecl; external libName ;
function JS_GetFrameThis(cx: PJSContext; fp: pJSStackFrame): pJSObject;cdecl; external libName ;
function JS_GetFrameFunction(cx: PJSContext; fp: pJSStackFrame): pJSFunction;cdecl; external libName ;
function JS_GetFrameFunctionObject(cx: PJSContext; fp: pJSStackFrame): pJSObject;cdecl; external libName ;
function JS_IsConstructorFrame(cx: PJSContext; fp: pJSStackFrame): jsbool;cdecl; external libName ;
function JS_IsDebuggerFrame(cx: PJSContext; fp: pJSStackFrame): jsbool;cdecl; external libName ;
function JS_GetFrameReturnValue(cx: PJSContext; fp: pJSStackFrame): jsval;cdecl; external libName ;
procedure JS_SetFrameReturnValue(cx: PJSContext; fp: pJSStackFrame;  rval: jsval);cdecl; external libName ;
function JS_GetFrameCalleeObject(cx: PJSContext; fp: pJSStackFrame): PJSObject;cdecl; external libName ;

//************************************************************************/

function JS_GetScriptFilename(cx: PJSContext; script: PJSObject): PAnsiChar;cdecl; external libName ;
function JS_GetScriptBaseLineNumber(cx: PJSContext; script: PJSObject): UIntN;cdecl; external libName ;
function JS_GetScriptLineExtent(cx: PJSContext; script: PJSObject): UIntN;cdecl; external libName ;
function JS_GetScriptVersion(cx: PJSContext; script: PJSObject): JSVersion;cdecl; external libName ;

procedure JS_SetNewScriptHook(rt: PJSRuntime; hook: JSNewScriptHook; callerdata: pointer);cdecl; external LibName name 'JS_SetNewScriptHookProc' ;

procedure JS_SetDestroyScriptHook(rt: PJSRuntime; hook: JSDestroyScriptHook; callerdata: pointer);cdecl; external LibName name 'JS_SetDestroyScriptHookProc' ;

function JS_EvaluateUCInStackFrame(cx: PJSContext; fp: pJSStackFrame;
                          chars: pjschar; length: uintN;
                          filename: pansichar;  lineno:uintN;
                          rval: pjsval): JSBool;cdecl; external libName ;

function JS_EvaluateInStackFrame(cx: PJSContext; fp: pJSStackFrame;
                        bytes: pansichar;  length: uintN;
                        filename:pansichar;  lineno: uintN;
                        rval: pjsval): JSBool;cdecl; external libName ;


///************************************************************************/

function JS_SetDebuggerHandler(rt: PJSRuntime;handler: JSTrapHandler; closure: pointer): JSBool;cdecl; external libName ;
function JS_SetSourceHandler(rt: PJSRuntime; handler: JSSourceHandler;closure: pointer): JSBool;cdecl; external libName ;
function JS_SetExecuteHook(rt: PJSRuntime; hook: JSInterpreterHook;closure: pointer): JSBool;cdecl; external libName ;
function JS_SetCallHook(rt: PJSRuntime; hook: JSInterpreterHook;closure: pointer): JSBool;cdecl; external libName ;
function JS_SetObjectHook(rt: PJSRuntime;hook: JSObjectHook;closure: pointer): JSBool;cdecl; external libName ;
function JS_SetThrowHook(rt: PJSRuntime; hook: JSTrapHandler;closure: pointer): JSBool;cdecl; external libName ;
function JS_SetDebugErrorHook(rt: PJSRuntime; hook: JSDebugErrorHook;closure: pointer): JSBool;cdecl; external libName ;

//************************************************************************/

function JS_GetObjectTotalSize(cx: PJSContext; obj: pJSObject): size_t;cdecl; external libName ;

function JS_GetFunctionTotalSize(cx: PJSContext; fun: pJSFunction): size_t;cdecl; external libName ;

function JS_GetScriptTotalSize(cx: PJSContext;script: PJSObject): size_t;cdecl; external libName ;

{*
 * Get the top-most running script on cx starting from fp, or from the top of
 * cx's frame stack if fp is null, and return its script filename flags.  If
 * the script has a null filename member, return JSFILENAME_NULL.
 *}
function JS_GetTopScriptFilenameFlags(cx: PJSContext; fp: pJSStackFrame): uint32;cdecl; external libName ;

{*
 * Get the script filename flags for the script.  If the script doesn't have a
 * filename, return JSFILENAME_NULL.
 *}
function JS_GetScriptFilenameFlags( script: PJSObject): uint32;cdecl; external libName ;

{*
 * Associate flags with a script filename prefix in rt, so that any subsequent
 * script compilation will inherit those flags if the script's filename is the
 * same as prefix, or if prefix is a substring of the script's filename.
 *
 * The API defines only one flag bit, JSFILENAME_SYSTEM, leaving the remaining
 * 31 bits up to the API client to define.  The union of all 32 bits must not
 * be a legal combination, however, in order to preserve JSFILENAME_NULL as a
 * unique value.  API clients may depend on JSFILENAME_SYSTEM being a set bit
 * in JSFILENAME_NULL -- a script with a null filename member is presumed to
 * be a "system" script.
 *}
function JS_FlagScriptFilenamePrefix(rt: pJSRuntime; prefix: pansichar; flags: uint32): jsbool;cdecl; external libName ;

//#define JSFILENAME_NULL         0xffffffff      /* null script filename */
//#define JSFILENAME_SYSTEM       0x00000001      /* "system" script, see below */

{*
 * Return true if obj is a "system" object, that is, one flagged by a prior
 * call to JS_FlagSystemObject(cx, obj).  What "system" means is up to the API
 * client, but it can be used to coordinate access control policies based on
 * script filenames and their prefixes, using JS_FlagScriptFilenamePrefix and
 * JS_GetTopScriptFilenameFlags.
 *}
function JS_IsSystemObject(cx: pJSContext; obj: pJSObject): jsbool;cdecl; external libName ;

{*
 * Flag obj as a "system" object.  The API client can flag system objects to
 * optimize access control checks.  The engine stores but does not interpret
 * the per-object flag set by this call.
 *}
procedure JS_FlagSystemObject(cx: pJSContext; obj: pJSObject);cdecl; external libName ;

(* Conversion routines *)
function JSStringToString(cx: PJSContext; str: PJSString): UnicodeString;
function JSStringToJSVal(str: PJSString): jsval;
function StringToJSString(cx: PJSContext; const str: UnicodeString): PJSString;
function StringToJSVal(cx: PJSContext; str: UnicodeString): jsval;
function JSObjectToJSVal(obj: PJSObject): jsval;
function DoubleToJSVal(dbl: Double): jsval;
function IntToJSVal(val: Integer): jsval; overload;
function BoolToJSVal(val: Boolean): jsval;

function JSValToDouble(cx: PJSContext; val: jsval): Double;
function JSValToObject(v: jsval): PJSObject;
function JSValToInt(val: jsval): Integer;
function JSValToJSString(val: jsval): PJSString;
function JSValToBoolean(v: jsval): Boolean;
function JSValToString(cx: PJSContext; val: jsval): UnicodeString;
function JSValMinInt: integer;
function JSValMaxInt: integer;

(* Validation routines *)
function JSValIsObject(v: jsval): Boolean;
function JSValIsObjectClass(cx: PJSContext; v: jsval; cl: TClass): Boolean;
function JSValIsNumber(v: jsval): Boolean;
function JSValIsInt(v: jsval): Boolean;
function JSValIsDouble(v: jsval): Boolean;
function JSValIsString(v: jsval): Boolean;
function JSValIsBoolean(v: jsval): Boolean;
function JSValIsNull(v: jsval): Boolean;
function JSValIsVoid(v: jsval): Boolean;

function CreateAnsiString(const Text: AnsiString): PAnsiChar; overload;
function CreateAnsiString(const Text: UnicodeString): PAnsiChar; overload;
function CreateWideString(const Text: AnsiString): PWideChar; overload;
function CreateWideString(const Text: UnicodeString): PWideChar; overload;
procedure SetReservedSlots(var Cls: JSClass; Reserve: Integer);

function JS_CALLEE(cx: PJSContext; vp: pjsval): jsval;inline;
function JS_ARGV(cx: PJSContext; vp: pjsval): jsval;inline;
function JS_ARGV_PTR(cx: PJSContext; vp: pjsval): pjsval;inline;
function JS_RVAL(cx: PJSContext; vp: pjsval): jsval;inline;
procedure JS_SET_RVAL(cx: PJSContext; vp: pjsval; v: jsval) {$ifndef debug};inline{$endif};

function JS_THIS(cx: PJSContext; vp: pjsval): jsval;
  //        ((vp)[0])
(*#define JS_THIS(cx,vp)          JS_ComputeThis(cx, vp)
#define JS_THIS_OBJECT(cx,vp)   (JSVAL_TO_OBJECT(JS_THIS(cx,vp)))
#define JS_ARGV(cx,vp)          ((vp) + 2)
#define JS_RVAL(cx,vp)          (*(vp))
#define JS_SET_RVAL(cx,vp,v)    (*(vp) = (v))
*)

implementation

uses Math;

function BUILD_JSVAL(tag: uint64; payload: uint32): jsval;
begin
  //((((uint64)(uint32)(tag)) << 32) | (uint32)(payload))
  result.asBits := (tag shl 32) or payload;
end;

{ SHR_INT32 }
function  shr_int32(i ,shift : integer ) : integer;
{$ifdef CPUX64}
asm
      // Source in ecx
      // Shift in dl
      mov eax,ecx
      mov rcx,rdx
      sar eax,cl
{$else}
begin
 asm
  mov eax ,dword ptr [i ]
  mov ecx ,dword ptr [shift ]
  sar eax ,cl
  mov dword ptr [result ] ,eax

 end;
{$endif}
end;

function JSStringToString(cx: PJSContext; str: PJSString): UnicodeString;
//var
//  s: ansiString;
begin
//  s := JS_GetStringChars(str);
  Result := UnicodeString(JS_GetStringCharsZ(cx, str));
end;

function StringToJSString(cx: PJSContext; const str: UnicodeString): PJSString;
begin
  Result := JS_NewUCStringCopyN(cx, PWideChar(str), Length(str));
end;


{$POINTERMATH ON}




{$ifdef CPUX64}

function JS_IsConstructing(cx: PJSContext; vp: pjsval): boolean;
var
  l: jsval_layout;
begin
  l.asBits := vp[1].asBits;
  result := (l.asBits shr JSVAL_TAG_SHIFT) = uint64(JSVAL_TAG_MAGIC);
end;

function STRING_TO_JSVAL_IMPL(str: PJSString): jsval_layout;  {$ifndef debug}inline;{$endif}
begin

    //JS_ASSERT(str);
    //jsval_layout l;
    //uint64 strBits = uint64(str);
    //JS_ASSERT(str);
    //JS_ASSERT((strBits >> JSVAL_TAG_SHIFT) == 0);
    result.asBits := uint64(str) or JSVAL_SHIFTED_TAG_STRING;
    //return l;

    //result.tag := JSVAL_TAG_STRING;
    //result.payload.str := str;
end;

function OBJECT_TO_JSVAL_IMPL(obj: PJSObject): jsval_layout;  {$ifndef debug}inline;{$endif}
begin

    //jsval_layout l;
    //uint64 objBits = (uint64)obj;
//    JS_ASSERT(obj);
//    JS_ASSERT((objBits >> JSVAL_TAG_SHIFT) == 0);
    result.asBits := uint64(obj) or JSVAL_SHIFTED_TAG_OBJECT;
    //return l;
end;

function DOUBLE_TO_JSVAL_IMPL(d: Double): jsval_layout; {$ifndef debug}inline;{$endif}
begin

    //jsval_layout l;
    result.asDouble := d;
    //JS_ASSERT(l.asBits <= JSVAL_SHIFTED_TAG_MAX_DOUBLE);
    //return l;
end;

function INT32_TO_JSVAL_IMPL(i: int32): jsval_layout;  {$ifndef debug}inline;{$endif}
begin

    //JS_ASSERT(obj);
    result.asBits := uint64(uint32(i)) or JSVAL_SHIFTED_TAG_INT32;

end;

function BOOLEAN_TO_JSVAL_IMPL(b: JSBool): jsval_layout;  {$ifndef debug}inline;{$endif}
begin

    //JS_ASSERT(obj);
    result.asBits := uint64(uint32(b)) or JSVAL_SHIFTED_TAG_BOOLEAN;
end;

function JSVAL_TO_OBJECT_IMPL(l: jsval_layout): PJSObject {$ifndef debug};inline{$endif};
var
  ptrBits: uint64;
begin

    //JS_ASSERT(obj);
    ptrBits := l.asBits and JSVAL_PAYLOAD_MASK;
    //JS_ASSERT((ptrBits & 0x7) == 0);
    result := PJSObject(ptrBits);
end;

function JSVAL_TO_INT32_IMPL(l: jsval_layout): int32 {$ifndef debug};inline{$endif};
begin

    //JS_ASSERT(obj);
    result := int32(l.asBits);
end;

function JSVAL_TO_STRING_IMPL(l: jsval_layout): PJSString {$ifndef debug};inline{$endif};
begin

    //JS_ASSERT(obj);
    result := PJSString(l.asBits and JSVAL_PAYLOAD_MASK);
end;

function JSVAL_TO_BOOLEAN_IMPL(l: jsval_layout): JSBool {$ifndef debug};inline{$endif};
begin

    //JS_ASSERT(obj);
    result := JSBool(l.asBits);
end;


function JSValIsObject(v: jsval): Boolean;
begin

    //l.asBits := v.asBits;
    //JS_ASSERT((uint32)l.s.tag <= (uint32)JSVAL_TAG_OBJECT);
    result := v.asBits >= JSVAL_SHIFTED_TAG_OBJECT;
    //return JSVAL_IS_OBJECT_OR_NULL_IMPL(l);
end;

function JSValIsInt(v: jsval): Boolean;
begin

    result := uint32(v.asBits shr JSVAL_TAG_SHIFT) = uint32(JSVAL_TAG_INT32);

end;

function JSValIsDouble(v: jsval): Boolean;
begin
    result := v.asBits <= JSVAL_SHIFTED_TAG_MAX_DOUBLE;
end;

function JSValIsString(v: jsval): Boolean;
begin
    result := uint32(v.asBits shr JSVAL_TAG_SHIFT) = uint32(JSVAL_TAG_STRING);
end;


function JSValIsBoolean(v: jsval): Boolean;
begin
    result := uint32(v.asBits shr JSVAL_TAG_SHIFT) = uint32(JSVAL_TAG_BOOLEAN);
end;


function JSValIsNull(v: jsval): Boolean;
begin
    result := v.asBits = JSVAL_SHIFTED_TAG_NULL;
end;


function JSValIsVoid(v: jsval): Boolean;
begin
    result := v.asBits = JSVAL_SHIFTED_TAG_UNDEFINED;
end;


function JS_THIS(cx: PJSContext; vp: pjsval): jsval;
begin
  // vp[1].asBits < JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_PRIMITIVE_SET;
  if vp[1].asBits < JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_PRIMITIVE_SET then
     result := JS_ComputeThis(cx, vp)
  else
      result := vp[1];
end;

{$else}


function JS_IsConstructing(cx: PJSContext; vp: pjsval): boolean;
var
  l: jsval_layout;
begin
  l.asBits := vp[1].asBits;
  result := l.tag = JSVAL_TAG_MAGIC;
end;

//function IMPL_TO_JSVAL(v) (v)

function STRING_TO_JSVAL_IMPL(str: PJSString): jsval_layout;  {$ifndef debug}inline;{$endif}
begin

    //JS_ASSERT(str);
    result.tag := JSVAL_TAG_STRING;
    result.payload.str := str;
end;

function DOUBLE_TO_JSVAL_IMPL(d: Double): jsval_layout; {$ifndef debug}inline;{$endif}
begin

    //JS_ASSERT(str);
    //result.tag := JSVAL_TAG_CLEAR;
    result.asDouble := d;
end;


function BOOLEAN_TO_JSVAL_IMPL(b: JSBool): jsval_layout;  {$ifndef debug}inline;{$endif}
begin

    //JS_ASSERT(obj);
    result.tag := JSVAL_TAG_BOOLEAN;
    result.payload.boo := b;
end;



function INT32_TO_JSVAL_IMPL(i: int32): jsval_layout;  {$ifndef debug}inline;{$endif}
begin
    //JS_ASSERT(obj);
    result.tag := JSVAL_TAG_INT32;
    result.payload.i32 := i;

end;

function OBJECT_TO_JSVAL_IMPL(obj: PJSObject): jsval_layout;  {$ifndef debug}inline;{$endif}
begin

    //jsval_layout l;
    //uint64 objBits = (uint64)obj;
//    JS_ASSERT(obj);
//    JS_ASSERT((objBits >> JSVAL_TAG_SHIFT) == 0);
    //jsval_layout l;
    //JS_ASSERT(obj);
    result.tag := JSVAL_TAG_OBJECT;
    result.payload.obj := obj;
//    return l;
//    result.asBits := uint64(obj) or JSVAL_SHIFTED_TAG_OBJECT;
    //return l;
end;



function JSVAL_TO_BOOLEAN_IMPL(l: jsval_layout): JSBool {$ifndef debug};inline{$endif};
begin

    //JS_ASSERT(obj);
    result := l.payload.boo;
end;

function JSVAL_TO_OBJECT_IMPL(l: jsval_layout): PJSObject {$ifndef debug};inline{$endif};
begin

    //JS_ASSERT(obj);
    result := l.payload.obj;
end;

function JSVAL_TO_INT32_IMPL(l: jsval_layout): int32 {$ifndef debug};inline{$endif};
begin

    //JS_ASSERT(obj);
    result := l.payload.i32;
end;

function JSVAL_TO_STRING_IMPL(l: jsval_layout): PJSString {$ifndef debug};inline{$endif};
begin

    //JS_ASSERT(obj);
    result := l.payload.str;
end;


function JSValIsObject(v: jsval): Boolean;
var
 l: jsval_layout;
begin

    l.asBits := v.asBits;
    //JS_ASSERT((uint32)l.s.tag <= (uint32)JSVAL_TAG_OBJECT);
    result := l.tag = JSVAL_TAG_OBJECT;
    //return JSVAL_IS_OBJECT_OR_NULL_IMPL(l);
end;

function JSValIsInt(v: jsval): Boolean;
var
 l: jsval_layout;
begin

    l.asBits := v.asBits;
    result := l.tag = JSVAL_TAG_INT32;

end;

function JSValIsDouble(v: jsval): Boolean;
var
 l: jsval_layout;
begin
    l.asBits := v.asBits;
    result := uint32(l.tag) <= uint32(JSVAL_TAG_CLEAR);
end;

function JSValIsString(v: jsval): Boolean;
var
 l: jsval_layout;
begin
    l.asBits := v.asBits;
    result := l.tag = JSVAL_TAG_STRING;
end;

function JSValIsBoolean(v: jsval): Boolean;
var
 l: jsval_layout;
begin
    l.asBits := v.asBits;
    result := l.tag = JSVAL_TAG_BOOLEAN;
end;

function JSValIsNull(v: jsval): Boolean;
var
 l: jsval_layout;
begin
    l.asBits := v.asBits;
    result := l.tag = JSVAL_TAG_NULL;
end;

function JSValIsVoid(v: jsval): Boolean;
var
 l: jsval_layout;
begin
    l.asBits := v.asBits;
    result := l.tag = JSVAL_TAG_UNDEFINED;
end;

function JS_THIS(cx: PJSContext; vp: pjsval): jsval;
begin
  if uint32(vp[1].tag) < uint32(JSVAL_UPPER_EXCL_TAG_OF_PRIMITIVE_SET) then
     result := JS_ComputeThis(cx, vp)
  else
      result := vp[1];
end;

{$endif}

function JSValIsObjectClass(cx: PJSContext; v: jsval; cl: TClass): Boolean;
var
  jsobj: PJSObject;
  p: Pointer;
begin
  if not JSValIsObject(v) then exit(false);
  jsobj := JSValToObject(v);
  p := JS_GetPrivate(cx, jsobj);
  if TObject(p) is cl then  exit(true);
  result := false;
  {
  begin
    Obj := TJSClass(p);
    Result := Obj.FNativeObj;
  end;
   }
end;

function JS_CALLEE(cx: PJSContext; vp: pjsval): jsval;
begin
   Result := vp[0];
end;

function JS_ARGV(cx: PJSContext; vp: pjsval): jsval;
begin
  inc(vp, 2);
  Result := vp^ ;
end;

function JS_ARGV_PTR(cx: PJSContext; vp: pjsval): pjsval;
begin
  inc(vp, 2);
  Result := vp;
end;

function JS_RVAL(cx: PJSContext; vp: pjsval): jsval;
begin
   Result := vp^;
end;

procedure  JS_SET_RVAL(cx: PJSContext; vp: pjsval; v: jsval);
begin
   vp^ := v;
end;



{$POINTERMATH OFF}


function JSStringToJSVal(str: PJSString): jsval;
begin
  Result := STRING_TO_JSVAL_IMPL(str);
(*  Result := jsval(str);
  if (not JSValIsString(Result)) then
    Result := Result or JSVAL_STRING;*)
end;

function StringToJSVal(cx: PJSContext; str: UnicodeString): jsval;
var
  jsstr: PJSString;
begin
  jsstr := JS_NewUCStringCopyN(cx, PWideChar(str), Length(str));
  Result := JSStringToJSVal(jsstr) ;
end;

function JSObjectToJSVal(obj: PJSObject): jsval;
begin
    if (obj <> nil) then
        exit(OBJECT_TO_JSVAL_IMPL(obj));
    Result := JSVAL_NULL;
//  Result := jsval(obj);
end;

function DoubleToJSVal( dbl: JSDouble): jsval;
begin
  result := DOUBLE_TO_JSVAL_IMPL(dbl);
//  JSValIsDouble(result);
//  JS_NewNumberValue(cx, dbl, @Result);
end;


function IntToJSVal(val: Integer): jsval;
begin
  Result := INT32_TO_JSVAL_IMPL(val);
end;


function BoolToJSVal(val: Boolean): jsval;
var
  tmp: jsbool;
begin
  if (val) then
    tmp := 1
  else
    tmp := 0;
  Result := BOOLEAN_TO_JSVAL_IMPL(tmp);
end;

function JSValToObject(v: jsval): PJSObject;
var
 l: jsval_layout;
begin
    //JS_ASSERT(JSVAL_IS_INT(v));
    l.asBits := v.asBits;
    result := JSVAL_TO_OBJECT_IMPL(l);
end;

function JSValToInt(val: jsval): Integer;
var
 l: jsval_layout;
begin
    //JS_ASSERT(JSVAL_IS_INT(v));
    l.asBits := val.asBits;
    result := JSVAL_TO_INT32_IMPL(l);
end;

function JSValToJSString(val: jsval): PJSString;
var
 l: jsval_layout;
begin
    //JS_ASSERT(JSVAL_IS_INT(v));
    l.asBits := val.asBits;
    result := JSVAL_TO_STRING_IMPL(l);
end;

function JSValToBoolean(v: jsval): Boolean;
var
 l: jsval_layout;
begin
    //JS_ASSERT(JSVAL_IS_INT(v));
    l.asBits := v.asBits;
    result := JSVAL_TO_BOOLEAN_IMPL(l) = 1;
end;


(*
#define JSVAL_INT_BITS          31
#define JSVAL_INT_POW2(n)       ((jsval)1 << (n))
#define JSVAL_INT_MIN           ((jsval)1 - JSVAL_INT_POW2(30))
#define JSVAL_INT_MAX           (JSVAL_INT_POW2(30) - 1)
#define INT_FITS_IN_JSVAL(i)    ((jsuint)((i)+JSVAL_INT_MAX) <= 2*JSVAL_INT_MAX)
#define JSVAL_TO_INT(v)         ((jsint)(v) >> 1)
#define INT_TO_JSVAL(i)         (((jsval)(i) << 1) | JSVAL_INT)
*)
function JSValIntPOW2(n: integer): integer;
begin
  Result := Integer(1) shl n;

end;

function JSValMinInt: integer;
begin
  result := (1 - JSValIntPOW2(30));
end;

function JSValMaxInt: integer;
begin
  result := (JSValIntPOW2(30) - 1);
end;

function JSValToDouble(cx: PJSContext; val: jsval): Double;
begin
  JS_ValueToNumber(cx,val,@Result);
end;

function JSValToString(cx: PJSContext; val: jsval): UnicodeString;
begin
   result := JSStringToString(cx, JS_ValueToString(cx, val));
end;

function JSValIsNumber(v: jsval): Boolean;
begin
  Result := (JSValIsInt(v) or JSValIsDouble(v));
end;

function CreateAnsiString(const Text: AnsiString): PAnsiChar;
var
  Size: Integer;
begin
  Size := Length(Text)+1;
{$ifdef unicode}
  Result := StrMove(AnsiStrAlloc(Size), PAnsiChar(Text), Size);
{$else}
  Result := StrMove(StrAlloc(Size), PAnsiChar(Text), Size);
{$endif}
end;

function CreateAnsiString(const Text: UnicodeString): PAnsiChar; overload;
begin
  Result := PAnsiChar(WideCharToString(PWideChar(Text)));
end;

function CreateWideString(const Text: AnsiString): PWideChar; overload;
begin
  Result := StringToOleStr(Text);
end;

function CreateWideString(const Text: UnicodeString): PWideChar; overload;
begin
  Result := PWideChar(Copy(Text, 1, Length(Text)));
end;

procedure SetReservedSlots(var Cls: JSClass; Reserve: Integer);
begin
  Cls.flags := Cls.flags or ((Reserve and JSCLASS_RESERVED_SLOTS_MASK) shl JSCLASS_RESERVED_SLOTS_SHIFT);
end;

var
  LOldNotifyHook, LOldFailureHook: TDelayedLoadHook;
  test : string;

function ImportName(const AProc: TDelayLoadProc): String {$ifndef debug};inline{$endif};
begin
  if AProc.fImportByName then
    Result := AProc.szProcName
  else
    Result := '#' + IntToStr(AProc.dwOrdinal);
end;
function MyDelayedLoadHook(dliNotify: dliNotification; pdli: PDelayLoadInfo): Pointer; stdcall;
begin
  { Write a message for each dli notification }
  case dliNotify of
    dliNoteStartProcessing:
      WriteLn('Started the delayed load session for "', pdli.szDll, '" DLL');
    dliNotePreLoadLibrary:
      WriteLn('Starting to load "', pdli.szDll, '" DLL');
    dliNotePreGetProcAddress:
      WriteLn('Want to get address of "', ImportName(pdli.dlp), '" in "', pdli.szDll, '" DLL');
    dliNoteEndProcessing:
      WriteLn('Ended the delaay load session for "', pdli.szDll, '" DLL');
    dliFailLoadLibrary:
      WriteLn('Failed to load "', pdli.szDll, '" DLL');
    dliFailGetProcAddress:
      WriteLn('Failed to get proc address for "', ImportName(pdli.dlp), '" in "', pdli.szDll, '" DLL');
  end;

  { Call the old hooks if they are not nil }
  { This is recommended to do in case the old hook do further processing }
  if dliNotify in [dliFailLoadLibrary, dliFailGetProcAddress] then
  begin
    if Assigned(LOldNotifyHook) then
      LOldFailureHook(dliNotify, pdli);
  end else
  begin
    if Assigned(LOldNotifyHook) then
      LOldNotifyHook(dliNotify, pdli);
  end;

  Result := nil;
end;

{Initialization
  LOldNotifyHook  := SetDliNotifyHook(MyDelayedLoadHook);
  LOldFailureHook := SetDliFailureHook(MyDelayedLoadHook);

  if @JS_ReportWarning <> nil then
  begin
     test := '';
  end;
}
//
//Initialization
//  JSVAL_NULL := BUILD_JSVAL(UInt32(JSVAL_TAG_NULL), 0);
(*
class operator jsval_layout.Implicit(const Value: boolean): jsval_layout;
begin
  Result := BoolToJSVal(Value);
end;

class operator jsval_layout.Implicit(const Value: integer): jsval_layout;
begin
  Result := IntToJSVal(Value);
end;

class operator jsval_layout.Implicit(const Value: int64): jsval_layout;
begin
  Result := DoubleToJSVal(Value);
end;

class operator jsval_layout.Implicit(const Value: double): jsval_layout;
begin
  Result := DoubleToJSVal(Value);
end;
*)
end.

