use bgfx_idl::*;
use convert_case::{Case, Casing};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Result, Write};

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
    "setViewTransform",
    "setTransform",
    "setViewName",
    "initCtor",
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

/// Run Rustfmt on generated file
fn run_rustfmt(filename: &str) {
    std::process::Command::new("rustfmt")
        .arg(filename)
        .output()
        .expect("failed to execute cargo fmt");
}

fn patch_string(input: &str, replace_data: &ReplaceFlagsEnums) -> Option<String> {
    if (!input.contains("`BGFX") && !input.contains("::Enum"))
        || input.contains("BGFX_CONFIG_MULTITHREADED")
        || input.contains("BGFX_CONFIG_MAX_BONES")
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

        if !output.contains("BGFX") {
            break;
        }

        if max_loop_count >= 4 {
            break;
            //panic!("Unable to get rid of BGFX_* in {}", output);
        }

        max_loop_count += 1;
    }

    output = output.replace('`', "]");

    Some(output)
}

fn patch_string_default(input: &str, replace_data: &ReplaceFlagsEnums) -> Option<String> {
    if (!input.contains("BGFX") && !input.contains("::Enum"))
        || input.contains("BGFX_CONFIG_MULTITHREADED")
        || input.contains("BGFX_CONFIG_MAX_BONES")
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

                if !output.contains('|') {
                    output.push_str(".bits()");
                }
            }
        }

        if !output.contains("BGFX") {
            break;
        }

        if max_loop_count >= 4 {
            panic!("Unable to get rid of BGFX_* in {}", output);
        }

        max_loop_count += 1;
    }

    if output.contains('|') {
        let mut new_output = String::with_capacity(256);
        let vals = output.split('|');
        let len = vals.clone().count();

        for (index, t) in vals.enumerate() {
            new_output.push_str(&t);
            new_output.push_str(".bits() as u64 ");

            if index != len - 1 {
                new_output.push_str(" | ");
            }
        }

        Some(new_output)
    } else {
        Some(output)
    }
}

fn generate_rust_comment<W: Write>(
    w: &mut W,
    text: &str,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    for line in text.lines() {
        if let Some(replace) = patch_string(&line, replace_data) {
            writeln!(w, "///{}", replace)?;
        } else {
            writeln!(w, "///{}", line)?;
        }
    }

    Ok(())
}

fn generate_rust_comment_bitflags<W: Write>(
    w: &mut W,
    text: &str,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    for line in text.lines() {
        if let Some(replace) = patch_string(&line, replace_data) {
            writeln!(w, "        ///{}", replace)?;
        } else {
            writeln!(w, "        ///{}", line)?;
        }
    }

    Ok(())
}

/// Get default value for a table
fn get_default_arg(table: &[TableEntry]) -> Option<String> {
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
    if let Some(t) = f.table.iter().find(|&t| t.name == "cname") {
        func_name = t.str_data.to_owned();
        replaced = true;
    }

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
    } else if let Ok(v) = count.parse::<usize>() {
        value_count = v;
    } else {
        panic!(
            "Unable to figure out array count for {}:{}",
            type_name, count
        );
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

fn generate_struct<W: Write>(
    w: &mut W,
    s: &Func,
    idl: &Idl,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    if SKIP_STRUCTS.iter().any(|n| n == &s.name.text) {
        return Ok(());
    }

    generate_rust_comment(w, &s.comments, replace_data)?;

    writeln!(w, "#[repr(C)]")?;
    writeln!(w, "pub struct {} {{", s.name.text)?;

    for e in &s.args {
        generate_rust_comment(w, &e.name_line.comment, replace_data)?;
        writeln!(
            w,
            "    pub {}: {},",
            e.name_line.text.to_case(Case::Snake),
            get_rust_type(&e.arg_type, idl, false)
        )?;
    }

    writeln!(w, "}}")
}

/// Some functions are of type setName (handle, ptr, len) and we special case those
/// as we can use &str directly then given than len is supported as input.
/// Returns true if setName function was generated, otherwise false
fn generate_set_name_func<W: Write>(w: &mut W, f: &Func, real_name: &str) -> Result<bool> {
    if f.name.text != "setName" {
        return Ok(false);
    }

    writeln!(w, "    pub fn set_name(&self, name: &str) {{")?;
    writeln!(w,
        "        unsafe {{ bgfx_sys::bgfx_{}(self.handle, name.as_ptr() as _, name.len() as i32) }}",
        real_name
    )?;
    writeln!(w, "    }}")?;

    Ok(true)
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

    if func_name.1 {
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
            fa.ret_value = get_rust_array(&type_name, &count, idl);
            fa.post_call = "std::mem::transmute(_ret)".to_owned();
        }

        _ => (),
    }

    let arg_count = count_default_angs(f);

    for (i, arg) in f.args.iter().skip(skip).enumerate() {
        // check if the arg is default null
        if arg_count >= 2 && arg.table.iter().any(|x| x.name == "default") {
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
                    output_type = format!("&{}", handle_name);
                    ffi_arg.push_str(".handle");
                    if s.is_pointer || s.is_ref {
                        ffi_arg = format!("&{}", ffi_arg);
                    }
                } else if name == "Memory" {
                    output_type.push_str(&name);
                    ffi_arg.push_str(".handle");
                } else {
                    if s.is_pointer || s.is_ref {
                        fa.body.push_str(&format!(
                            "let _{} = std::mem::transmute({});",
                            arg_name, arg_name
                        ));
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

fn print_args<W: Write>(w: &mut W, args: &[String]) -> Result<()> {
    let len = args.len();
    for (i, v) in args.iter().enumerate() {
        if i == len - 1 {
            write!(w, "{}", v)?;
        } else {
            write!(w, "{}, ", v)?;
        }
    }

    Ok(())
}

/// Generate default parameter struct
fn generate_default_params<W: Write>(
    w: &mut W,
    f: &Func,
    name: &str,
    idl: &Idl,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    let name = format!("{}Args", name.to_case(Case::UpperCamel));

    writeln!(w, "pub struct {} {{", name)?;

    let mut default_args = Vec::with_capacity(f.args.len());

    for e in &f.args {
        // get the default argument
        if let Some(da) = get_default_arg(&e.table) {
            generate_rust_comment(w, &e.name_line.comment, replace_data)?;
            let arg_name = e.name_line.text.to_case(Case::Snake);
            let rust_type = get_rust_type(&e.arg_type, idl, true);

            if da == "None" {
                writeln!(w, "pub {}: Option<{}>,", arg_name, rust_type)?;
            } else {
                writeln!(w, "pub {}: {},", arg_name, rust_type)?;
            }

            if let Some(p) = patch_string_default(&da, replace_data) {
                default_args.push((arg_name, p));
            } else {
                default_args.push((arg_name, da));
            }
        }
    }

    writeln!(w, "}}\n")?;

    writeln!(w, "impl Default for {} {{", name)?;
    writeln!(w, "fn default() -> {} {{", name)?;
    writeln!(w, "{} {{", name)?;

    for a in default_args {
        writeln!(w, "{}: {},", a.0, a.1)?;
    }

    writeln!(w, "}}}}}}\n")
}

/// General function implementation
fn generate_func<W: Write>(
    w: &mut W,
    f: &Func,
    idl: &Idl,
    func_mode: FunctionMode,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    // if function has been marked for skipping we skip it
    if SKIP_FUNCS.iter().any(|n| n == &f.name.text) {
        return Ok(());
    }

    let func_name = get_func_name(f).0;

    // in global mode we should not
    if func_mode == FunctionMode::Global {
        // these are bound to intance functions
        if f.name.text == "destroy" || f.name.text == "setName" {
            return Ok(());
        }
    }

    for a in &f.args {
        write!(w, "/// * `{}`:", a.name_line.text.to_case(Case::Snake))?;
        let mut skip_first_com = true;

        for line in a.name_line.text.lines().skip(1) {
            if let Some(replace) = patch_string(&line, replace_data) {
                if skip_first_com {
                    writeln!(w, " {}", replace)?;
                    skip_first_com = false;
                } else {
                    writeln!(w, "///{}", replace)?;
                }
            } else if skip_first_com {
                writeln!(w, " {}", line)?;
                skip_first_com = false;
            } else {
                writeln!(w, "///{}", line)?;
            }
        }

        writeln!(w)?;

        generate_rust_comment(w, &a.name_line.comment, replace_data)?;
    }

    if func_mode == FunctionMode::Handle {
        // generate check name function if it is one
        if generate_set_name_func(w, f, &func_name)? {
            return Ok(());
        }
    }

    let arg_count = f.args.len();

    match func_mode {
        FunctionMode::Method => {
            if arg_count == 0 {
                write!(w, "    pub fn {}(&self", func_name)?;
            } else {
                write!(w, "    pub fn {}(&self, ", func_name)?;
            }
        }

        FunctionMode::Global => write!(w, "pub fn {}(", func_name)?,
        _ => (),
    }

    let ffi_name = get_ffi_call_name(f);

    let mut call_args = String::with_capacity(256);

    if func_mode == FunctionMode::Handle {
        if arg_count <= 1 {
            write!(w, "    pub fn {}(&self", func_name)?;
            call_args.push_str("self.handle");
        } else {
            write!(w, "    pub fn {}(&self, ", func_name)?;
            call_args.push_str("self.handle, ");
        }
    }

    let fa = get_func_call_args(f, &func_name, idl, func_mode);

    print_args(w, &fa.func_args)?;

    if !fa.ret_value.is_empty() {
        writeln!(w, ") -> {} {{", fa.ret_value)?;
    } else {
        writeln!(w, ") {{")?;
    }

    writeln!(w, "unsafe {{")?;

    if !fa.body.is_empty() {
        writeln!(w, "{}", fa.body)?;
    }

    if let VarType::Unknown(_) = f.return_type.var_type {
        write!(w, "bgfx_sys::{}(", ffi_name)?;
    } else {
        write!(w, "let _ret = bgfx_sys::{}(", ffi_name)?;
    }

    print_args(w, &fa.ffi_args)?;
    writeln!(w, ");")?;

    if !fa.post_call.is_empty() {
        writeln!(w, "{}", fa.post_call)?;
    }

    writeln!(w, "}}\n}}")
}

fn generate_funcs_for_struct<W: Write>(
    w: &mut W,
    name: &str,
    idl: &Idl,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    writeln!(w, "impl {} {{", name)?;
    writeln!(w, "    pub fn new() -> {} {{", name)?;
    writeln!(w, "        let t = MaybeUninit::<{}>::zeroed();", name)?;
    writeln!(w, "        let t = unsafe {{ t.assume_init() }};")?;

    // special case for bgfx_init_ctor
    if name == "Init" {
    	writeln!(w, "unsafe {{")?;
        writeln!(w, "let _init = std::mem::transmute(&t);")?;
        writeln!(w, "bgfx_sys::bgfx_init_ctor(_init); }}")?;
    }

    writeln!(w, "        t")?;
    writeln!(w, "    }}\n")?;

    for f in &idl.funcs {
        if f.class.text == name {
            generate_func(w, f, idl, FunctionMode::Method, replace_data)?;
        }
    }

    writeln!(w, "}}\n")
}

fn generate_handles<W: Write>(w: &mut W, h: &Typedef) -> Result<()> {
    let len = h.type_line.text.len() - 6;

    writeln!(w, "#[derive(Clone, Debug)]")?;
    writeln!(w, "pub struct {} {{ ", &h.type_line.text[..len])?;
    writeln!(
        w,
        "    handle: bgfx_sys::bgfx_{}_t,",
        h.type_line.text.to_case(Case::Snake)
    )?;
    writeln!(w, "}}\n")
}

// generates implementations for the handle structs
fn generate_handle_impl<W: Write>(
    w: &mut W,
    idl: &Idl,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    for h in &idl.handles {
        let h_type = &h.type_line.text;
        let len = h_type.len() - 6;

        writeln!(w, "impl {} {{ ", &h_type[..len])?;

        // find matching handle name for the first argument
        for f in &idl.funcs {
            // destroy will be handled in the next loop
            if !f.class.text.is_empty() || f.name.text == "destroy" {
                continue;
            }

            if !f.args.is_empty() && &f.args[0].type_name == h_type {
                generate_func(w, f, idl, FunctionMode::Handle, replace_data)?;
            } else if &f.return_name_line.text == h_type {
                generate_func(w, f, idl, FunctionMode::Global, replace_data)?;
            }
        }

        writeln!(w, "}}\n")?;

        // generate Drop if there is one for the type
        for f in &idl.funcs {
            if f.name.text != "destroy" || &f.args[0].type_name != h_type {
                continue;
            }

            let func_name = get_func_name(&f).0;

            writeln!(w, "impl Drop for {} {{", &h_type[..len])?;
            writeln!(w, "fn drop(&mut self) {{")?;
            writeln!(
                w,
                "unsafe {{ bgfx_sys::bgfx_{}(self.handle); }}}}}}\n",
                func_name
            )?;
        }
    }

    Ok(())
}

fn generate_def_func_args<W: Write>(
    w: &mut W,
    lookup: &mut HashMap<String, u32>,
    f: &Func,
    idl: &Idl,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    if SKIP_FUNCS.iter().any(|n| n == &f.name.text) {
        return Ok(());
    }

    // check if we have default args for the function
    let arg_count = count_default_angs(f);

    // generate default args
    if arg_count <= 1 {
        return Ok(());
    }

    let func_name = get_func_name(f);

    if let Some(count) = lookup.get(&func_name.0) {
		assert!(*count == arg_count);
		return Ok(());
    }

    generate_default_params(w, f, &func_name.0, idl, replace_data)?;

    lookup.insert(func_name.0, arg_count);

    Ok(())
}

// generate the structs for the default arguments
fn generate_default_arg_structs<W: Write>(
    w: &mut W,
    idl: &Idl,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    let mut lookup = HashMap::with_capacity(256);

    for f in &idl.funcs {
        generate_def_func_args(w, &mut lookup, &f, idl, replace_data)?;
    }

    Ok(())
}

// Generate enums
fn generate_enum<W: Write>(w: &mut W, f: &Flag, replace_data: &ReplaceFlagsEnums) -> Result<()> {
    generate_rust_comment(w, &f.comments, replace_data)?;

    writeln!(w, "#[repr(u32)]")?;
    writeln!(w, "#[derive(Clone, Copy, PartialEq, Debug)]")?;
    writeln!(w, "pub enum {} {{", f.name.text)?;

    for e in &f.entries {
        if !e.name_line.comment.is_empty() {
            write!(w, "    ///{}", e.name_line.comment)?;
        }
        writeln!(w, "    {},", e.name_line.text)?;
    }

    writeln!(w, "    /// Number of entries in the enum")?;
    writeln!(w, "    Count,")?;

    writeln!(w, "}}\n")
}

// Generate enums
fn generate_bitflags<W: Write>(
    w: &mut W,
    f: &Flag,
    replace_data: &ReplaceFlagsEnums,
) -> Result<()> {
    if f.entries.is_empty() {
        return Ok(());
    }

    writeln!(w, "bitflags! {{")?;
    generate_rust_comment(w, &f.comments, replace_data)?;
    writeln!(w, "    pub struct {}Flags : u{} {{", f.name.text, f.size)?;

    for e in &f.entries {
        generate_rust_comment_bitflags(w, &e.name_line.comment, replace_data)?;

        let name = e.name_line.text.to_case(Case::UpperSnake);

        if !e.str_value.is_empty() {
            writeln!(
                w,
                "        const {} = bgfx_sys::{} as _;",
                name, e.str_value
            )?;
        } else {
            writeln!(w, "        const {} = {},", name, e.value)?;
        }
    }

    //writeln!(w, "        /// Combined mask");
    //writeln!(w, "        const MASK = 0x{:x};", mask);
    writeln!(w, "    }}")?;
    writeln!(w, "}}\n")
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

fn output_wrapper<W: Write>(w: &mut W, data: &Idl) -> Result<()> {
    writeln!(w, "use std::mem::MaybeUninit;")?;
    writeln!(w, "use core::ffi::c_void;\n")?;

    let replace_data = get_flags_enums_replace_args(&data);

    for e in &data.enums {
        generate_enum(w, e, &replace_data)?;
    }

    for f in &data.flags {
        generate_bitflags(w, f, &replace_data)?;
    }

    for h in &data.handles {
        generate_handles(w, &h)?;
    }

    generate_default_arg_structs(w, &data, &replace_data)?;

    for s in &data.structs {
        generate_struct(w, &s, &data, &replace_data)?;
    }

    generate_handle_impl(w, &data, &replace_data)?;

    for s in &data.structs {
        generate_funcs_for_struct(w, &s.name.text, &data, &replace_data)?;
    }

    for f in &data.funcs {
        if f.class.text.is_empty() {
            generate_func(w, &f, &data, FunctionMode::Global, &replace_data)?;
        }
    }

    writeln!(w, "{}", MANUAL_IMPL)
}

// takes the static input file and patches it up with the dynamic calls
fn write_shared_api_file<W: Write>(w: &mut W, static_lib: &str) -> Result<()> {
	let mut header = false;

	for line in static_lib.lines() {
		if line.is_empty() && !header {
			writeln!(w, "STATIC g_vtbl: *const bgfx_sys::bgfx_interface_vtbl_t = std::ptr::null();\n")?;
			writeln!(w, "pub fn set_interface(face: *const bgfx_sys::bgfx_interface_vtbl_t) {{")?;
			writeln!(w, "g_vtbl = face;")?;
			writeln!(w, "}}")?;
			header = true;
		}

		if !line.contains("bgfx_sys::bgfx_") || !line.contains('(') || !line.contains(')') {
			writeln!(w, "{}", line)?;
			continue;
		}

		// translate this style: bgfx_sys::bgfx_set_compute_index_buffer(stage, handle.handle, access as _);
		// to (*g_vtbl.bgfx_set_compute_index_buffer).unwrap()(stage, handle.handle, access as _);

		let start_func_name = line.find("::").unwrap() + 2;
		let end = line.find('(').unwrap();
		let func_name = &line[start_func_name..end];
		let new_name = format!("(*g_vtbl.{}).unwrap()", func_name);
		let replace_name = format!("bgfx_sys::{}", func_name);
		let new_line = line.replace(&replace_name, &new_name);

		writeln!(w, "{}", new_line)?;
	}

	Ok(())
}

fn main() {
    let data = bgfx_idl::parse_bgfx_idl("/home/emoon/temp/foo.idl").unwrap();

    let static_filename = "/home/emoon/code/projects/bgfx-rs/src/static_lib.rs";
    let shared_filename = "/home/emoon/code/projects/bgfx-rs/src/shared_lib.rs";

    {
        let mut regular_output = BufWriter::new(File::create(&static_filename).unwrap());
        output_wrapper(&mut regular_output, &data).unwrap();
    }

    run_rustfmt(&static_filename);

    {
    	let t = std::fs::read_to_string(static_filename).unwrap();
        let mut output = BufWriter::new(File::create(&shared_filename).unwrap());
        write_shared_api_file(&mut output, &t).unwrap();
    }

    run_rustfmt(&shared_filename);

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

/// * `x`:
/// Position x from the left corner of the window.
/// * `y`:
/// Position y from the top corner of the window.
/// * `attr`:
/// Color palette. Where top 4-bits represent index of background, and bottom
/// 4-bits represent foreground color from standard VGA text palette (ANSI escape codes).
/// * `text`: Text to be displayed
pub fn dbg_text(x: u16, y: u16, attr: u8, text: &str) {
    unsafe {
    	let c_text = std::ffi::CString::new(text).unwrap();
        bgfx_sys::bgfx_dbg_text_printf(x, y, attr, c_text.as_ptr());
    }
}

/// * `id`:
/// View id.
/// * `view`:
/// View matrix.
/// * `proj`:
/// Projection matrix.
pub fn set_view_transform(id: ViewId, view: &[f32; 16], proj: &[f32; 16]) {
    unsafe {
    	let _view = std::mem::transmute(view);
    	let _proj = std::mem::transmute(proj);
        bgfx_sys::bgfx_set_view_transform(id, _view, _proj);
    }
}

/// * `mtx`:
/// Pointer to first matrix in array.
/// * `num`:
/// Number of matrices in array.
pub fn set_transform(mtx: &[f32; 16], num: u16) -> u32 {
    unsafe {
    	let _mtx = std::mem::transmute(mtx);
        bgfx_sys::bgfx_set_transform(_mtx, num)
    }
}

";
