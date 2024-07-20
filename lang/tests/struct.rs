use simplevm::Register::*;

mod common;
use common::*;

#[test]
fn struct_in_frame() {
    let test = "
type Foo := struct {
    int x,
    int y,
    int z,
};

void main() {
    let Foo f;
    f.x := 0x34c;
    f.y := 0x2e6;
    f.z := 0x1ff;
    return 5;
}
   ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 5);
}

#[test]
fn struct_field_set() {
    let test = "
type Array := struct {
  *void ptr,
  int len,
};

int main() {
  let *Array y := makearray();
  setlen(y, 82);
  return y.len;
}

*Array makearray() {
  let *Array out := 0x1000;
  out.ptr := 0x2000;
  out.len := 0;
  return out;
}

void setlen(*Array a, int len) {
  a.len := len; 
}
";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 82);
}

#[test]
fn struct_nested_fields() {
    let test = "
type Bar := struct {
    int x, 
};

type Foo := struct {
  int x,
  Bar bar,
};

int main() {
    let Foo foo;
    foo.bar.x := 5;
    return foo.bar.x;
}
";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 5);
}

#[test]
fn struct_more_nested_fields() {
    let test = "
type Baz := struct {
    int x,
    int y,
    int z,
    struct {
        int x,
        int y,
        struct {
            int x,
            int y,
        } deeper,
    } deep,
};

type Bar := struct {
    int x, 
    int y,
    Baz baz,
};

type Foo := struct {
  int x,
  Bar bar,
};

int main() {
    let Foo foo;
    foo.bar.baz.deep.deeper.y := 51;
    return foo.bar.baz.deep.deeper.y;
}
";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 51);
}

#[test]
fn struct_ptr_nested_fields() {
    let test = "
type Baz := struct {
    int x,
    int y,
    int z,
    struct {
        int x,
        int y,
        struct {
            int x,
            int y,
        } deeper,
    } deep,
};

type Bar := struct {
    int x, 
    int y,
    *Baz baz,
};

type Foo := struct {
  int x,
  *Bar bar,
};

int main() {
    let Foo foo;
    foo.bar := 0x1000;
    foo.bar.baz := 0x1100;
    foo.bar.baz.deep.deeper.y := 99;
    return foo.bar.baz.deep.deeper.y;
}
";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 99);
}

#[test]
fn nested_field_address() {
    let test = "
type Foo := struct {
    int x,
    int y,
    int z,
};

int main() {
    let Foo foo;
    let a := &foo.y;
    *a := 72;
    return foo.y;
}
    ";
    let vm = run_program(test).unwrap();
    assert_eq!(vm.get_register(A), 72);
}
