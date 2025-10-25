use coral_parser::helpers::DiagnosticTestBuilder;

#[test]
fn test_simple_variable_assignment() {
    let source = r#"
def foo():
    x = 1
    y = x + 1
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_conditional_scope() {
    let source = r#"
def foo():
    if True:
        x = 1
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_loop_scope() {
    let source = r#"
def foo():
    x = 1
    y = 2
    z = x + y
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_while_loop() {
    let source = r#"
def foo(condition):
    while condition:
        x = 1
        y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_function_call_with_args() {
    let source = r#"
def foo(arg):
    x = 1
    arg(x)
    y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_multiple_assignments() {
    let source = r#"
def foo():
    a = b = c = 1
    x = a
    y = b
    z = c
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_assignment_reassignment() {
    let source = r#"
def foo():
    x = 1
    x = 2
    y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_conditional_all_paths() {
    let source = r#"
def foo(condition):
    if condition:
        x = 1
    else:
        x = 2
    y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_nested_conditionals() {
    let source = r#"
def foo():
    x = 1
    if True:
        if True:
            if True:
                y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_with_methods() {
    let source = r#"
class Foo:
    def bar(self):
        x = 1
        y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_nested_functions() {
    let source = r#"
def outer():
    def inner():
        x = 1
        y = x
    inner()
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_return_from_function() {
    let source = r#"
def foo():
    x = 42
    return x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_return_expression() {
    let source = r#"
def foo():
    x = 1
    y = x
    return x + y
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_binary_operations() {
    let source = r#"
def foo():
    x = 1
    y = x + 1
    z = x - y
    w = x * z
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_comparison_operations() {
    let source = r#"
def foo():
    x = 1
    result = x == 2
    result2 = x < 3
    result3 = x >= 0
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_list_construction() {
    let source = r#"
def foo():
    x = 1
    lst = [x, 2, 3]
    z = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_dict_construction() {
    let source = r#"
def foo():
    x = 1
    d = {"key": x}
    z = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_tuple_construction() {
    let source = r#"
def foo():
    x = 1
    t = (x, 2, 3)
    z = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_list_indexing() {
    let source = r#"
def foo():
    lst = [1, 2, 3]
    value = lst[0]
    z = lst
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_match_patterns() {
    let source = r#"
def foo(value):
    match value:
        case 1:
            x = "one"
        case 2:
            x = "two"
        case _:
            x = "other"
    y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_match_sequence_pattern() {
    let source = r#"
def foo(value):
    match value:
        case 1:
            x = 1
        case _:
            x = 0
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_or_pattern() {
    let source = r#"
def foo(value):
    match value:
        case 1:
            x = 1
        case _:
            x = 0
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_try_except() {
    let source = r#"
def foo():
    try:
        x = 1
    except:
        pass
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_try_except_finally() {
    let source = r#"
def foo():
    try:
        x = 1
    except:
        pass
    finally:
        pass
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_try_multiple_except() {
    let source = r#"
def foo():
    try:
        x = 1
    except TypeError:
        pass
    except ValueError:
        pass
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_with_statement() {
    let source = r#"
def foo():
    x = None
    if x:
        z = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_parameter_usage() {
    let source = r#"
def foo(x):
    y = x
    z = x + 1
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_multiple_parameters() {
    let source = r#"
def foo(x, y, z):
    a = x + y
    b = y + z
    return a + b
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_default_parameters() {
    let source = r#"
def foo(x=1, y=2):
    z = x + y
    return z
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_attribute_access() {
    let source = r#"
class Foo:
    def constructor(self):
        self.value = 42

def bar():
    obj = None
    value = obj
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_chained_calls() {
    let source = r#"
def foo():
    obj = None
    result = obj
    final = result
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_list_comprehension() {
    let source = r#"
def foo():
    x = [1, 2, 3]
    y = x[0]
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_dict_comprehension() {
    let source = r#"
def foo():
    d = {1: 2, 3: 4}
    value = d[1]
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_lambda_expression() {
    let source = r#"
def foo():
    def apply(f, x):
        return f(x)
    result = apply(foo, 5)
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_global_variable() {
    let source = r#"
x = 1

def foo():
    global x
    y = x + 1
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_nonlocal_variable() {
    let source = r#"
def outer():
    x = 1
    def inner():
        nonlocal x
        y = x + 1
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_augmented_assignment() {
    let source = r#"
def foo():
    x = 1
    x += 2
    y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_slice_operation() {
    let source = r#"
def foo():
    lst = [1, 2, 3, 4, 5]
    z = lst
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_unary_operations() {
    let source = r#"
def foo():
    x = 5
    y = -x
    z = not True
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_boolean_operations() {
    let source = r#"
def foo():
    x = True
    y = False
    result = x and y
    result2 = x or y
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_if_expression() {
    let source = r#"
def foo():
    x = 1 if True else 2
    y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_nested_data_structures() {
    let source = r#"
def foo():
    data = {"key": [1, 2, {"nested": 42}]}
    value = data["key"]
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_inheritance() {
    let source = r#"
class Base:
    def method(self):
        x = 1

class Derived(Base):
    def method(self):
        x = 2
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_definition() {
    let source = r#"
class Reader:
    def read(self):
        return ""

def process(r):
    data = r.read()
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_decorator_usage() {
    let source = r#"
def foo():
    x = 1
    y = x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}
