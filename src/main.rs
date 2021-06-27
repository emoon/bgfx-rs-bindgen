use bgfx_idl::*;
use convert_case::{Case, Casing};
use std::collections::HashMap;

// list of functions to skip that are manually implemented
static SKIP_FUNCS: &[&str] = &[
	"getDirectAccessPtr",
	"getRendererName",
	"getSupportedRenderers",
	"setPaletteColor",
	"topologyConvert",
	"topologySortTriList",
    "VertexLayout.convert",
    "VertexLayout.decode",
    "VertexLayout.pack",
    "alloc",
    "convert",
    "copy",
    "dbgTextPrintf",
    "dbgTextPrintfVargs",
    "decode",
    "getShaderUniforms",
    "makeRef",
    "overrideInternal",
    "pack",
    "readTexture",
    "setViewOrder",
    "vertexConvert",
    "vertexPack",
    "vertexUnpack",
    "weldVertices",
];
static SKIP_STRUCTS: &[&str] = &["Memory"];

/// Used for replacing BGFX_<NAME>_<NAME> comments and default values
#[derive(Default, Debug)]
struct ReplaceFlagsEnums {
    /// like BGFX_CAPS_, Caps
    flags: Vec<(String, String)>,
    /// like BGFX_CAPS_, Caps
    enums: Vec<(String, String)>,
}

fn patch_string(input: &str, replace_data: &ReplaceFlagsEnums) -> Option<String> {
    if (input.find("`BGFX").is_none() && input.find("::Enum").is_none())
        || input.find("BGFX_CONFIG_MULTITHREADED").is_some()
        || input.find("BGFX_CONFIG_MAX_BONES").is_some()
    {
        return None;
    }

    let mut output = input.to_owned();
    let mut max_loop_count = 0;

    loop {
        for t in &replace_data.enums {
            if output.contains(&t.1) {
                output = output.replace(&format!("`{}::Enum`", t.1), &format!("[{}]", t.1));
            }
        }

        for t in &replace_data.flags {
            if output.contains(&t.0) {
                output = output.replace(&format!("`{}*`", t.0), &format!("[{}Flags]", t.1));
                output = output.replace(&format!("`{}", t.0), &format!("[{}Flags::", t.1));
            }
        }

        if output.find("BGFX").is_none() {
            break;
        }

        if max_loop_count >= 4 {
            panic!("Unable to get rid of BGFX_* in {}", output);
        }

        max_loop_count += 1;
    }

    output = output.replace('`', "]");

    Some(output)
}

fn patch_string_default(input: &str, replace_data: &ReplaceFlagsEnums) -> Option<String> {
    if (input.find("BGFX").is_none() && input.find("::Enum").is_none())
        || input.find("BGFX_CONFIG_MULTITHREADED").is_some()
        || input.find("BGFX_CONFIG_MAX_BONES").is_some()
    {
        return None;
    }

    let mut output = input.to_owned();
    let mut max_loop_count = 0;

    loop {
        for t in &replace_data.enums {
            if output.contains(&t.0) {
                output = output.replace(&t.0, &format!("{} as _", t.1));
            }
        }

        for t in &replace_data.flags {
            if output.contains(&t.0) {
                output = output.replace(&t.0, &format!("{}Flags::", t.1));

                if !output.contains("|") {
                    output.push_str(".bits()");
                }
            }
        }

        if output.find("BGFX").is_none() {
            break;
        }

        if max_loop_count >= 4 {
            panic!("Unable to get rid of BGFX_* in {}", output);
        }

        max_loop_count += 1;
    }

    if output.contains("|") {
        let mut new_output = String::with_capacity(256);
        let vals = output.split("|");
        let len = vals.clone().count();
        let mut index = 0;

        for t in vals {
            new_output.push_str(&t);
            new_output.push_str(".bits() as u64 ");

            if index != len - 1 {
                new_output.push_str(" | ");
            }

            index += 1;
        }

        Some(new_output)
    } else {
        Some(output)
    }
}

fn generate_rust_comment(text: &str, replace_data: &ReplaceFlagsEnums) {
    for line in text.lines() {
        if let Some(replace) = patch_string(&line, replace_data) {
            println!("///{}", replace);
        } else {
            println!("///{}", line);
        }
    }
}

fn generate_rust_comment_bitflags(text: &str, replace_data: &ReplaceFlagsEnums) {
    for line in text.lines() {
        if let Some(replace) = patch_string(&line, replace_data) {
            println!("        ///{}", replace);
        } else {
            println!("        ///{}", line);
        }
    }
}

/// Get default value for a table
fn get_default_arg(table: &Vec<TableEntry>) -> Option<String> {
    for t in table {
        if t.name == "default" {
            if !t.str_data.is_empty() {
                match t.str_data.as_str() {
                    "UINT8_MAX" => return Some("std::u8::MAX".to_owned()),
                    "UINT16_MAX" => return Some("std::u16::MAX".to_owned()),
                    "INT32_MAX" => return Some("std::i32::MAX".to_owned()),
                    "NULL" => return Some("None".to_owned()),
                    "false" => return Some("false".to_owned()),
                    "true" => return Some("true".to_owned()),
                    _ => (),
                }

                return Some(t.str_data.to_owned());
            } else {
                return Some(t.num.unwrap().to_string());
            }
        }
    }

    None
}

/// Count the number of default arguments exists in a function
fn count_default_angs(f: &Func) -> u32 {
    f.args
        .iter()
        .map(|a| {
            if get_default_arg(&a.table).is_some() {
                1u32
            } else {
                0u32
            }
        })
        .sum::<u32>()
}

/// Get the name of a function
fn get_func_name(f: &Func) -> (String, bool) {
    let mut func_name = get_snake_name(&f.name.text);
    let mut replaced = false;

    // if we have a replacement name
    f.table.iter().find(|&t| t.name == "cname").map(|t| {
        func_name = t.str_data.to_owned();
        replaced = true;
    });

    (func_name, replaced)
}

// look for an enum by name and get how many entries it has. This code
// is expected to always work find an entry so will panic if it doesn't
fn get_enum_count(name: &str, idl: &Idl) -> usize {
    for e in &idl.enums {
        if e.name.text == name {
            return e.entries.len();
        }
    }

    panic!("Enum {} wasn't found", name);
}

fn get_rust_primitive_type(s: &str) -> Option<&'static str> {
    match s {
        "char" => Some("i8"),
        "bool" => Some("bool"),
        "uint8_t" => Some("u8"),
        "uint16_t" => Some("u16"),
        "uint32_t" => Some("u32"),
        "uint64_t" => Some("u64"),
        "int8_t" => Some("i8"),
        "int16_t" => Some("i16"),
        "int32_t" => Some("i32"),
        "int64_t" => Some("i64"),
        "float" => Some("f32"),
        "void" => Some("c_void"),
        _ => None,
    }
}

fn get_rust_array(type_name: &str, count: &str, idl: &Idl) -> String {
    let value_count;

    // found the count for the array
    if let Some(enum_count) = count.find("::Count") {
        value_count = get_enum_count(&count[..enum_count], idl);
    } else {
        if let Ok(v) = count.parse::<usize>() {
            value_count = v;
        } else {
            panic!(
                "Unable to figure out array count for {}:{}",
                type_name, count
            );
        }
    }

    if let Some(rust_type_name) = get_rust_primitive_type(type_name) {
        format!("[{}; {}usize]", rust_type_name, value_count)
    } else {
        // if we didn't find a matching we assemu we can use it directly
        format!("[{}; {}usize]", type_name, value_count)
    }

    // first we try to parse the number if it's a regular number
    // otherwise we need
}

fn get_rust_type(s: &Type, idl: &Idl, is_arg: bool) -> String {
    let mut output_type = String::new();

    if s.is_ref || (s.is_pointer && is_arg) {
        output_type.push('&')
    } else if s.is_pointer {
        output_type.push('*');

        if !s.is_output {
            output_type.push_str("const ");
        }
    }

    if s.is_output {
        output_type.push_str("mut ");
    }

    match &s.var_type {
        VarType::Primitive(name) => {
            output_type.push_str(get_rust_primitive_type(&name).unwrap());
            output_type
        }

        VarType::Struct(name) => {
			if let Some((handle_name, _)) = name.rsplit_once("Handle") {
                output_type = handle_name.to_owned();
            } else if name == "Memory" {
                output_type = "Memory".to_owned();
            } else {
                output_type.push_str(&name);
            }
            output_type
        }

        VarType::Enum(name) => {
            output_type.push_str(&name);
            output_type
        }

        VarType::Array(type_name, count) => get_rust_array(&type_name, &count, idl),

        _ => panic!("Unable to figure out Rust type for {:#?}", s),
    }
}

// This is used to patch comments/text such as:
// `BGFX_STATE_BLEND_*` to [StateBlendFlags] (comment)
// `BGFX_CAPS_ATTRIBUTE` to [CapsFlags::ATTRBITUE] (comment)
// `BGFX_CAPS_ATTRIBUTE` to CapsFlags::ATTRBITUE.bits() (default value)
//fn patch_comment_const

fn generate_struct(s: &Func, idl: &Idl, replace_data: &ReplaceFlagsEnums) {
    if SKIP_STRUCTS.iter().find(|n| *n == &s.name.text).is_some() {
        return;
    }

    generate_rust_comment(&s.comments, replace_data);

    println!("#[repr(C)]");
    println!("pub struct {} {{", s.name.text);

    for e in &s.args {
        generate_rust_comment(&e.name_line.comment, replace_data);
        println!(
            "    pub {}: {},",
            e.name_line.text.to_case(Case::Snake),
            get_rust_type(&e.arg_type, idl, false)
        );
    }

    println!("}}");
}

/// Some functions are of type setName (handle, ptr, len) and we special case those
/// as we can use &str directly then given than len is supported as input.
/// Returns true if setName function was generated, otherwise false
fn generate_set_name_func(f: &Func, real_name: &str) -> bool {
    if f.name.text != "setName" {
        return false;
    }

    println!("    pub fn set_name(&self, name: &str) {{");
    println!(
        "        unsafe {{ bgfx_sys::bgfx_{}(self.handle, name.as_ptr() as _, name.len() as i32) }}",
        real_name
    );
    println!("    }}");

    true
}

/// Select how to generate the function
#[derive(Clone, Copy, PartialEq)]
enum FunctionMode {
    Method,
    Global,
    Handle,
}

fn get_snake_name(name: &str) -> String {
    let mut output = String::with_capacity(256);
    let mut prev = '_';

    for c in name.chars() {
        if c.is_ascii_uppercase() {
            let mut t = c;
            t.make_ascii_lowercase();

            if !prev.is_numeric() && prev != '_' {
                output.push('_');
            }

            output.push(t);
        } else {
            if c.is_numeric() && prev != '_' {
                output.push('_');
            }
            output.push(c);
        }

        prev = c;
    }

    //let mut name = name.to_case(Case::Snake);
    /*
    // special case: 2D gets translated to 2_d and we want 2d

    let len = name.len();
    let mut remove_incorrect_space = false;

    if name.len() >= 3 {
        let bytes = name.as_bytes();
        let nr = bytes[len - 3];
        if (nr >= b'0') && (nr < b'9') {
            remove_incorrect_space = true;
        }
    }

    if remove_incorrect_space {
        name.remove(len - 2);
    }
    */

    output
}

fn get_ffi_call_name(f: &Func) -> String {
    let mut name = String::with_capacity(256);
    let func_name = get_func_name(f);

    name.push_str("bgfx_");
    if !f.class.text.is_empty() {
        let mut t = format!("{}_", &f.class.text);

        if t == "VertexLayoutBuilder_" {
            t = "VertexLayout_".to_owned();
        }

        name.push_str(&t);
    }

    name.push_str(&func_name.0);

    if func_name.1 == true {
        name.to_lowercase()
    } else {
        get_snake_name(&name)
    }
}

/// Rules for input parameter translation
///
/// bool/uint16_t/uint32_t/etc -> bool/u16/u32/etc, can pass directly to ffi func
///
/// ----------------------------------------------------------------------------
/// input: .format "TextureFormat::Enum"
///
/// args:      format: TextureFormat
/// body:
/// ffi call:  format.bits()
/// ----------------------------------------------------------------------------
/// input: .mem "const Memory*"
///
/// args:      format: &Memory
/// body:
/// ffi call:  format.handle
/// ----------------------------------------------------------------------------
/// input: .mem "const Memory*" { default = NULL }
/// args:      format: Optional<Memory>
/// body:      let _mem = if let Some(f) = format { f.handle } else { null() };
/// ffi call:  _mem
/// ----------------------------------------------------------------------------
/// input: .flags "uint64_t" { default = "BGFX_TEXTURE_NONE|BGFX_SAMPLER_NONE" }
/// args:      flags: u64
/// body:      let _flags = TextureFlags::NONE.bits() | SamplerFlags::NONE.bits();
/// ffi call:  flags
/// ----------------------------------------------------------------------------
/// input: .handle "Texture<*>"
/// args:      &self
/// ffi call:  self.handle
/// ----------------------------------------------------------------------------
/// input: .name "const char*", .len
/// args:      name: &str
/// ffi call:  name.as_ptr(), name.len() as i32
///
/// ----------------------------------------------------------------------------
/// input: .name "const char*", .len
/// args:      name: &str
/// ffi call:  name.as_ptr(), name.len() as i32
///
/// ----------------------------------------------------------------------------
/// input: .data "void*"
/// pre_f      <X: Sized>
/// args:      data: &mut [T]
/// ffi call:  data.as_ptr()
/// ----------------------------------------------------------------------------
/// input: .data "const void*"
/// pre_f      <X: Sized>
/// args:      data: &[T]
/// ffi call:  data.as_ptr()
/// ----------------------------------------------------------------------------
/// input: .data "float[4]" { out }
/// args:      data: &mut [f32; 4]
/// ffi call:  data.as_ptr()
/// ----------------------------------------------------------------------------
/// input: .data "const float[4]"
/// args:      data: &[f32; 4]
/// ffi call:  data.as_ptr()
/// ----------------------------------------------------------------------------
/// input: .layout "const VertexLayout&"
/// args:      layout: &VertexLayout
/// ffi call:  layout.handle
/// ----------------------------------------------------------------------------
/// input: .name "const char*"
/// args:      name: &str
/// body:      let _name = CStrWrapper(name).unwrap();
/// ffi call:  _name.as_ptr()

/// Return values
///
/// ----------------------------------------------------------------------------
/// input:    void*
/// ret:      -> c_void
/// mark call unsafe
/// ffi call:
/// ----------------------------------------------------------------------------
/// input:    VertexLayout&
/// ret:      -> Self
/// ----------------------------------------------------------------------------
/// input:    "const Stats*"
/// ret:      -> Option<&'static Stats>
/// body:     ffi_call(...).as_ref()
/// ----------------------------------------------------------------------------
/// input:    "RenderType::Enum"
/// ret:      -> RenderType
/// body:     RenderType::from_bits(ffi_call(...))

#[derive(Default)]
struct FuncArgs {
    func_args: Vec<String>,
    ffi_args: Vec<String>,
    body: String,
    post_call: String,
    ret_value: String,
}

/// Generate the function setup when there are default arguments
fn generate_func_default_args(
    fa: &mut FuncArgs,
    f: &Func,
    start: usize,
    func_name: &str,
    _idl: &Idl,
) {
    // Setup the args with is the collection of the default arguments inside a struct
    fa.func_args.push(format!(
        "params: {}Args",
        func_name.to_case(Case::UpperCamel)
    ));

    for arg in f.args.iter().skip(start) {
        let arg_name = arg.name_line.text.to_case(Case::Snake);

        let is_none = get_default_arg(&arg.table).map(|t| t == "None").is_some();
        let s = &arg.arg_type;

        match &s.var_type {
            VarType::Primitive(_name) => {
                fa.ffi_args.push(format!("params.{}", arg_name.to_owned()));
            }

            VarType::Struct(_name) => {
                if is_none {
                    fa.body.push_str(&format!("let _{} = if let Some(h) = params.{} {{ h.handle }} else {{ std::ptr::null() }};", arg_name, arg_name));
                    fa.ffi_args.push(format!("_{}", arg_name));
                } else {
                    fa.ffi_args.push(format!("params.{}", arg_name));
                }
            }

            VarType::Enum(name) => {
                // TODO: Support inout?
                //fa.ffi_args.push(format!("params.{}.bits()", arg_name));
                if name.contains("Flags") {
                	fa.ffi_args.push(format!("params.{}.bits()", arg_name));
                } else {
                	fa.ffi_args.push(format!("params.{} as _", arg_name));
                }
            }

            VarType::Array(_type_name, _count) => {
                fa.ffi_args.push(format!("params.{}", arg_name));
            }

            _ => panic!("Unable to figure out Rust type for {:#?}", s),
        }
    }
}

/// Constructs the arguments and body for a ffi function call
/// Returns func_args, body and args for ffi function
fn get_func_call_args(f: &Func, func_name: &str, idl: &Idl, func_mode: FunctionMode) -> FuncArgs {
    let mut fa = FuncArgs::default();
    let mut skip = 0;

    match func_mode {
		FunctionMode::Method => {
			fa.body.push_str("let _self = std::mem::transmute(self);");
			fa.ffi_args.push("_self".to_owned());
		}

		FunctionMode::Handle => {
			fa.ffi_args.push("self.handle".to_owned());
			skip = 1;
		}

		_ => (),
    }

	// setup return value
	// TODO: Reduce code?

	match &f.return_type.var_type {
		VarType::Primitive(name) => {
			fa.ret_value = get_rust_primitive_type(&name).unwrap().to_owned();
			fa.post_call = "_ret".to_owned();
		}

		VarType::Struct(name) => {
			// Translate *Handle to *
			if let Some((handle_name, _)) = name.rsplit_once("Handle") {
				fa.ret_value = handle_name.to_owned();
				fa.post_call = format!("{} {{ handle: _ret }}", handle_name);
			} else if name == "VertexLayoutBuilder" {
				// special case vertex layout builder
				fa.ret_value = "&Self".to_owned();
				fa.post_call = "self".to_owned();
			} else {
				if f.return_type.is_pointer || f.return_type.is_ref {
					fa.ret_value = format!("&'static {}", name);
				} else {
					fa.ret_value = name.to_owned();
				}

				fa.post_call = "std::mem::transmute(_ret)".to_owned();
			}
		}

		VarType::Enum(name) => {
			fa.ret_value = name.to_owned();
			fa.post_call = "std::mem::transmute(_ret)".to_owned();
		}

		VarType::Array(type_name, count) => {
			fa.ret_value = get_rust_array(&type_name, &count, idl).to_owned();
			fa.post_call = "std::mem::transmute(_ret)".to_owned();
		}

		_ => (),
	}

    let arg_count = count_default_angs(f);

    for (i, arg) in f.args.iter().skip(skip).enumerate() {
        // check if the arg is default null
        if arg_count >= 2 && arg.table.iter().find(|x| x.name == "default").is_some() {
            generate_func_default_args(&mut fa, f, i, func_name, idl);
            return fa;
        }

        let arg_name = arg.name_line.text.to_case(Case::Snake);
        let s = &arg.arg_type;

        let mut output_type = String::new();
        let mut ffi_arg = arg_name.clone();

        if s.is_pointer || s.is_ref {
            output_type.push('&')
        }

        if s.is_output {
            output_type.push_str("mut ");
        }

        match &s.var_type {
            VarType::Primitive(name) => {
                output_type.push_str(get_rust_primitive_type(&name).unwrap());
            }

            VarType::Struct(name) => {
                // Translate *Handle to *
                if let Some((handle_name, _)) = name.rsplit_once("Handle") {
                    output_type.push_str(handle_name);
                    ffi_arg.push_str(".handle");
					if s.is_pointer || s.is_ref {
						ffi_arg = format!("&{}", ffi_arg);
					}
                } else if name == "Memory" {
                    output_type.push_str(&name);
                    ffi_arg.push_str(".handle");
                } else {
                	if s.is_pointer || s.is_ref {
                		fa.body.push_str(&format!("let _{} = std::mem::transmute({});", arg_name, arg_name));
                		ffi_arg = format!("_{}", arg_name);
                	}
                    output_type.push_str(&name);
                }
            }

            VarType::Enum(name) => {
                output_type.push_str(&name);
                ffi_arg.push_str(" as _");
            }

            VarType::Array(type_name, count) => {
                output_type.push_str(&get_rust_array(&type_name, &count, idl));
            }

            _ => panic!("Unable to figure out Rust type for {:#?}", s),
        }

        fa.ffi_args.push(ffi_arg);
        fa.func_args.push(format!("{}: {}", arg_name, output_type));
    }

    fa
}

fn print_args(args: &Vec<String>) {
    let len = args.len();
    for (i, v) in args.iter().enumerate() {
        if i == len - 1 {
            print!("{}", v)
        } else {
            print!("{}, ", v)
        }
    }
}

/// Generate default parameter struct
fn generate_default_params(f: &Func, name: &str, idl: &Idl, replace_data: &ReplaceFlagsEnums) {
    let name = format!("{}Args", name.to_case(Case::UpperCamel));

    println!("pub struct {} {{", name);

    let mut default_args = Vec::with_capacity(f.args.len());

    for e in &f.args {
        // get the default argument
        if let Some(da) = get_default_arg(&e.table) {
            generate_rust_comment(&e.name_line.comment, replace_data);
            let arg_name = e.name_line.text.to_case(Case::Snake);
            let rust_type = get_rust_type(&e.arg_type, idl, true);

            if da == "None" {
                println!("pub {}: Option<{}>,", arg_name, rust_type);
            } else {
                println!("pub {}: {},", arg_name, rust_type);
            }

            if let Some(p) = patch_string_default(&da, replace_data) {
                default_args.push((arg_name, p));
            } else {
                default_args.push((arg_name, da));
            }
        }
    }

    println!("}}\n");

    println!("impl Default for {} {{", name);
    println!("fn default() -> {} {{", name);
    println!("{} {{", name);

    for a in default_args {
        println!("{}: {},", a.0, a.1);
    }

    println!("}}}}}}\n");
}

/// General function implementation
fn generate_func(f: &Func, idl: &Idl, func_mode: FunctionMode, replace_data: &ReplaceFlagsEnums) {
    // if function has been marked for skipping we skip it
    if SKIP_FUNCS.iter().find(|n| *n == &f.name.text).is_some() {
        return;
    }

    let func_name = get_func_name(f).0;

    generate_rust_comment(&f.comments, replace_data);

    if func_mode == FunctionMode::Handle {
        // generate check name function if it is one
        if generate_set_name_func(f, &func_name) {
            return;
        }
    } else {
        if func_name.contains("set_name") {
            return;
        }
    }

    let arg_count = f.args.len();

    match func_mode {
        FunctionMode::Method => {
            if arg_count == 0 {
                print!("    pub fn {}(&self", func_name);
            } else {
                print!("    pub fn {}(&self, ", func_name);
            }
        }

        FunctionMode::Global => print!("pub fn {}(", func_name),
        _ => (),
    }

    let ffi_name = get_ffi_call_name(f);

    let mut call_args = String::with_capacity(256);

    if func_mode == FunctionMode::Handle {
        if arg_count <= 1 {
            print!("    pub fn {}(&self", func_name);
            call_args.push_str("self.handle");
        } else {
            print!("    pub fn {}(&self, ", func_name);
            call_args.push_str("self.handle, ");
        }
    }

    let fa = get_func_call_args(f, &func_name, idl, func_mode);

    print_args(&fa.func_args);

    if !fa.ret_value.is_empty() {
    	println!(") -> {} {{", fa.ret_value);
    } else {
    	println!(") {{");
    }

    println!("unsafe {{");

    if !fa.body.is_empty() {
        println!("{}", fa.body);
    }

    if let VarType::Unknown(_) = f.return_type.var_type {
    	print!("bgfx_sys::{}(", ffi_name);
    } else {
    	print!("let _ret = bgfx_sys::{}(", ffi_name);
    }

    print_args(&fa.ffi_args);
    println!(");");

    if !fa.post_call.is_empty() {
    	println!("{}", fa.post_call);
    }

    println!("}}\n}}");
}

fn generate_funcs_for_struct(name: &str, idl: &Idl, replace_data: &ReplaceFlagsEnums) {
    println!("impl {} {{", name);
    println!("    pub fn new() -> {} {{", name);
    println!("        let t = MaybeUninit::<{}>::zeroed();", name);
    println!("        let t = unsafe {{ t.assume_init() }};");
    println!("        t");
    println!("    }}\n");

    for f in &idl.funcs {
        if f.class.text == name {
            generate_func(f, idl, FunctionMode::Method, replace_data);
        }
    }

    println!("}}\n");
}

fn generate_handles(h: &Typedef) {
    let len = h.type_line.text.len() - 6;

    println!("#[derive(Copy, Clone, Debug)]");
    println!("pub struct {} {{ ", &h.type_line.text[..len]);
    println!("    handle: bgfx_sys::bgfx_{}_t,", h.type_line.text.to_case(Case::Snake));
    println!("}}\n");
}

// generates implementations for the handle structs
fn generate_handle_impl(idl: &Idl, replace_data: &ReplaceFlagsEnums) {
    for h in &idl.handles {
        let h_type = &h.type_line.text;
        let len = h_type.len() - 6;

        println!("impl {} {{ ", &h_type[..len]);

        // find matching handle name for the first argument
        for f in &idl.funcs {
            if !f.class.text.is_empty() {
                continue;
            }

            if !f.args.is_empty() && &f.args[0].type_name == h_type {
                generate_func(f, idl, FunctionMode::Handle, replace_data);
            } else if &f.return_name_line.text == h_type {
                generate_func(f, idl, FunctionMode::Global, replace_data);
            }
        }

        println!("}}\n");
    }
}

fn generate_def_func_args(
    lookup: &mut HashMap<String, u32>,
    f: &Func,
    idl: &Idl,
    replace_data: &ReplaceFlagsEnums,
) {
    if SKIP_FUNCS.iter().find(|n| *n == &f.name.text).is_some() {
        return;
    }

    // check if we have default args for the function
    let arg_count = count_default_angs(f);

    // generate default args
    if arg_count <= 1 {
        return;
    }

    let func_name = get_func_name(f);

    match lookup.get(&func_name.0) {
        Some(count) => {
            assert!(*count == arg_count);
            return;
        }
        _ => (),
    }

    generate_default_params(f, &func_name.0, idl, replace_data);

    lookup.insert(func_name.0, arg_count);
}

// generate the structs for the default arguments
fn generate_default_arg_structs(idl: &Idl, replace_data: &ReplaceFlagsEnums) {
    let mut lookup = HashMap::with_capacity(256);

    for f in &idl.funcs {
        generate_def_func_args(&mut lookup, &f, idl, replace_data);
    }
}

// Generate enums
fn generate_enum(f: &Flag, replace_data: &ReplaceFlagsEnums) {
    generate_rust_comment(&f.comments, replace_data);

    println!("#[repr(u32)]");
    println!("#[derive(Clone, Copy, PartialEq, Debug)]");
    println!("pub enum {} {{", f.name.text);

    for e in &f.entries {
        if !e.name_line.comment.is_empty() {
            print!("    ///{}", e.name_line.comment);
        }
        println!("    {},", e.name_line.text);
    }

    println!("    /// Number of entries in the enum");
    println!("    Count,");

    println!("}}\n");
}

// Generate enums
fn generate_bitflags(f: &Flag, replace_data: &ReplaceFlagsEnums) {
    if f.entries.is_empty() {
        return;
    }

    println!("bitflags! {{");
    generate_rust_comment(&f.comments, replace_data);
    println!("    pub struct {}Flags : u{} {{", f.name.text, f.size);

    for e in &f.entries {
        generate_rust_comment_bitflags(&e.name_line.comment, replace_data);

        let name = e.name_line.text.to_case(Case::UpperSnake);

        if !e.str_value.is_empty() {
            println!("        const {} = bgfx_sys::{} as _;", name, e.str_value);
        } else {
            println!("        const {} = {},", name, e.value);
        }
    }

    //println!("        /// Combined mask");
    //println!("        const MASK = 0x{:x};", mask);
    println!("    }}");
    println!("}}\n");
}

// Returns two arrays
fn get_flags_enums_replace_args(idl: &Idl) -> ReplaceFlagsEnums {
    let mut output = ReplaceFlagsEnums::default();

    for e in &idl.enums {
        let bgfx_name = format!("BGFX_{}_", e.name.text.to_case(Case::UpperSnake));
        output.enums.push((bgfx_name, e.name.text.to_owned()));
    }

    output.enums.sort();

    for e in &idl.flags {
        let bgfx_name = format!("BGFX_{}_", e.name.text.to_case(Case::UpperSnake));
        output.flags.push((bgfx_name, e.name.text.to_owned()));
    }

    // sort so the longest names are first as the replacement won't work corretly otherwise
    output.enums.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
    output.flags.sort_by(|a, b| b.0.len().cmp(&a.0.len()));

    output
}

fn main() {
    let data = bgfx_idl::parse_bgfx_idl("/home/emoon/temp/foo.idl").unwrap();

    println!("#[macro_use]");
    println!("extern crate bitflags;\n");
    println!("use std::mem::MaybeUninit;");
    println!("use core::ffi::c_void;\n");

    let replace_data = get_flags_enums_replace_args(&data);

    data.enums
        .iter()
        .for_each(|e| generate_enum(e, &replace_data));
    data.flags
        .iter()
        .for_each(|e| generate_bitflags(e, &replace_data));

    for h in &data.handles {
        generate_handles(&h);
    }

    generate_default_arg_structs(&data, &replace_data);

    for s in &data.structs {
        generate_struct(&s, &data, &replace_data);
    }

    generate_handle_impl(&data, &replace_data);

    for s in &data.structs {
        generate_funcs_for_struct(&s.name.text, &data, &replace_data);
    }

    for f in &data.funcs {
        if f.class.text.is_empty() {
            generate_func(&f, &data, FunctionMode::Global, &replace_data);
        }
    }

    println!("{}", MANUAL_IMPL);

    //generate_funcs_for_struct("VertexLayout", &data);
}

// for various reasons here is a list of manual implementations for some functions
static MANUAL_IMPL: &str = "

type ViewId = u16;

/// Returns the number of uniforms and uniform handles used inside a shader.
///
/// Notice that only non-predefined uniforms are returned.

impl Shader {
	//pub fn get_uniforms(&self, uniforms: &mut [Uniform]) -> u16 {
    //	unsafe { bgfx_sys::bgfx_get_shader_uniforms(self.handle, uniforms.as_ptr(), uniforms.len() as u16) }
	//}
}

/// bgfx-managed buffer of memory.
///
/// It can be created by either copying existing data through [`copy(...)`], or by referencing
/// existing memory directly through [`reference(...)`].
///
/// [`copy(...)`]: #method.copy
/// [`reference(...)`]: #method.reference
#[derive(Copy, Clone)]
pub struct Memory {
    handle: *const bgfx_sys::bgfx_memory_t,
}

impl Memory {
    /// Copies the source data into a new bgfx-managed buffer.
    ///
    /// **IMPORTANT:** If this buffer is never passed into a bgfx call, the memory will never be
    /// freed, and will leak.
    #[inline]
    pub fn copy<T>(data: &[T]) -> Memory {
        unsafe {
            let handle = bgfx_sys::bgfx_copy(data.as_ptr() as *const c_void,
                                           std::mem::size_of_val(data) as u32);
            Memory { handle }
        }
    }

    /// Creates a reference to the source data for passing into bgfx. When using this constructor
    /// over the `copy` call, no copy will be created. bgfx will read the source memory directly.
    ///
    /// *Note* That the data passed to this function must be keep alive during the whole duration
    /// of the program and is only really recommended for static data unless you know you know
    /// what you are doing. Thus this function is marked as unsafe because of this reason.
    #[inline]
    pub unsafe fn reference<T>(data: &[T]) -> Memory {
        let handle = bgfx_sys::bgfx_make_ref(data.as_ptr() as *const c_void,
                                           std::mem::size_of_val(data) as u32);
        Memory { handle }
    }
}

";
