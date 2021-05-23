use bgfx_idl::*;
use std::borrow::Cow;
use convert_case::{Case, Casing};
/*
struct VertexLayout {
    data: bgfx::bgfx_vertex_layout_s;
}

impl VertexLayout {
    pub fn new() -> VertexLayout {
        VertexLayout { data: bgfx::bgfx_vertex_layout_s::default(); }
    }

    pub fn begin(&self, render_type: RenderType) -> &self {
        unsafe { bgfx_vertex_layout_begin(self.data as *const void, render_type);
        self
    }

    pub fn add(attrib: Attrib, num: u8, type: AttribType, normalized: bool, as_int: bool) -> &self {
        unsafe { bgfx_vertex_layout_add(self.data as *const void, render_type);
    }

    pub fn has(attrib: Attrib) -> bool {
        unsafe { bgfx_vertex_layout_has(self.data as *const void) }
    }

    pub fn add(attrib: Attrib, num: u8, type: AttribType, normalized: bool, as_int: bool) -> &self {
        unsafe { bgfx_vertex_layout_add(self.data as *const void, render_type);
    }
}
*/

fn generate_rust_comment(text: &str, indent_level: usize) {
    let lines = text.lines();

    for line in lines {
        println!("{:indent$}/// {}", "", line, indent = indent_level);
    }
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
        "char" => Some("s8"),
        "bool" => Some("bool"),
        "uint8_t" => Some("u8"),
        "uint16_t" => Some("u16"),
        "uint32_t" => Some("u32"),
        "uint64_t" => Some("u32"),
        "int8_t" => Some("s8"),
        "int16_t" => Some("s16"),
        "int32_t" => Some("s32"),
        "int64_t" => Some("s32"),
        "float" => Some("f32"),
        "void" => Some("std::os::raw::c_void"),
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
            panic!("Unable to figure out array count for {}:{}", type_name, count);
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

fn get_rust_type(s: &Type, idl: &Idl) -> String {
    let mut output_type = String::new();

    if s.is_ref {
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
        },

        VarType::Struct(name) => {
            output_type.push_str(&name);
            output_type
        },

        VarType::Enum(name) => {
            output_type.push_str(&name);
            output_type
        },

        VarType::Array(type_name, count) => get_rust_array(&type_name, &count, idl),

        _ => panic!("Unable to figure out Rust type for {:#?}", s),
    }
}

fn generate_struct(s: &Func, idl: &Idl) {
    generate_rust_comment(&s.comments, 0);

    println!("#[repr(C)]");
    println!("struct {} {{", s.name.text);

    for e in &s.args {
        generate_rust_comment(&e.name_line.comment, 4);
        println!("    {}: {},", e.name_line.text.to_case(Case::Snake), get_rust_type(&e.arg_type, idl));
    }

    println!("}}");
}

fn generate_impl_func(f: &Func, idl: &Idl) {
    let mut func_name = f.name.text.to_case(Case::Snake);

    for t in &f.table {
        if t.name == "cname" {
            func_name = t.str_data.to_owned();
            break;
        }
    }

    generate_rust_comment(&f.comments, 4);
    print!("    pub fn {}(&self", func_name);

    for arg in &f.args {
        print!(", {}: {}", arg.name_line.text.to_case(Case::Snake), get_rust_type(&arg.arg_type, idl))
    }

    println!(") {{");

    print!("      unsafe {{ bgfx_sys::bgfx_{}(self as *const void", func_name);

    for arg in &f.args {
        print!(", {}", arg.name_line.text.to_case(Case::Snake));
    }

    println!(")  }}\n");

    println!("    }}\n");

    // TODO: generate call to bgfx C code
}

fn generate_funcs_for_struct(name: &str, idl: &Idl) {
    println!("impl {} {{", name);
    println!("    pub fn new() -> {} {{", name);
    println!("        let t = MaybeUninit::<{}>::zeroed();", name);
    println!("        unsafe {{ t.assume_init(); }}");
    println!("        t");
    println!("    }}\n");

    for f in &idl.funcs {
        if f.class.text == name {
            generate_impl_func(f, idl);
        }
    }

    println!("}}\n");
}

fn generate_global_func(f: &Func, idl: &Idl) {
    generate_rust_comment(&f.comments, 0);

    let mut func_name = f.name.text.to_case(Case::Snake);

    for t in &f.table {
        if t.name == "cname" {
            func_name = t.str_data.to_owned();
            break;
        }
    }

    print!("pub fn {}(", func_name);

    let arg_count = f.args.len();
    let mut call_args = String::new();

    for (i, arg) in f.args.iter().enumerate() {
        let arg_name = arg.name_line.text.to_case(Case::Snake);
        if i == arg_count - 1 {
            call_args.push_str(&arg_name);
            print!("{}: {}", arg_name, get_rust_type(&arg.arg_type, idl))
        } else {
            call_args.push_str(&arg_name);
            call_args.push_str(", ");
            print!("{}: {}, ", arg_name, get_rust_type(&arg.arg_type, idl))
        }
    }

    println!(") {{");

    println!("    unsafe {{ bgfx_sys::bgfx_{}({}) }}", func_name, call_args);
    println!("}}\n");
}


fn main() {
    let data = bgfx_idl::parse_bgfx_idl("/home/emoon/temp/foo.idl").unwrap();

    for s in &data.structs {
        //if s.name.text == "VertexLayout" {
            generate_struct(&s, &data);
        //}
    }

    for s in &data.structs {
        generate_funcs_for_struct(&s.name.text, &data);
        //if s.name.text == "VertexLayout" {
            //generate_struct(&s, &data);
        //}
    }

    for f in &data.funcs {
        if f.class.text.is_empty() {
            generate_global_func(&f, &data);
        }
    }

    //generate_funcs_for_struct("VertexLayout", &data);
}

/*
struct VertexLayout {
    data: bgfx::bgfx_vertex_layout_s;
}

impl VertexLayout {
    pub fn new() -> VertexLayout {
        VertexLayout { data: bgfx::bgfx_vertex_layout_s::default(); }
    }

    pub fn begin(&self, render_type: RenderType) -> &self {
        unsafe { bgfx_vertex_layout_begin(self.data as *const void, render_type);
        self
    }

    pub fn add(attrib: Attrib, num: u8, type: AttribType, normalized: bool, as_int: bool) -> &self {
        unsafe { bgfx_vertex_layout_add(self.data as *const void, render_type);
    }

    pub fn has(attrib: Attrib) -> bool {
        unsafe { bgfx_vertex_layout_has(self.data as *const void) }
    }

    pub fn add(attrib: Attrib, num: u8, type: AttribType, normalized: bool, as_int: bool) -> &self {
        unsafe { bgfx_vertex_layout_add(self.data as *const void, render_type);
    }
}
*/


