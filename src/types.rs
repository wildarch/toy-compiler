use parse::{Op, Lit, Ident, Expr};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum Primitive {
    Bool,
    Float,
    Int,
    Str,
    List(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Primitive(Primitive),
    Free,
}

#[derive(Debug)]
enum TypeError {
    Mismatch(Type, Type),
    UnsupportedOp(Type, Op, Type),
    MissingIdent(Ident),
}

fn get_type(expr: &Expr, constraints: &HashMap<Ident, Type>) -> Result<Type, TypeError> {
    match expr {
        Expr::Lit(lit) => match lit {
            Lit::Bool(_) => Ok(Type::Primitive(Primitive::Bool)),
            Lit::Float(_) => Ok(Type::Primitive(Primitive::Float)),
            Lit::Int(_) => Ok(Type::Primitive(Primitive::Int)),
            Lit::Str(_) => Ok(Type::Primitive(Primitive::Str)),
            Lit::List(items) => {
                if let Some(head) = items.first() {
                    let head = get_type(head, constraints)?;
                    for item in items.iter() {
                        let ty = get_type(item, constraints)?;
                        if ty != head {
                            return Err(TypeError::Mismatch(ty, head));
                        }
                    }
                    Ok(Type::Primitive(Primitive::List(Box::new(head))))
                } else {
                    Ok(Type::Primitive(Primitive::List(Box::new(Type::Free))))
                }
            }
        }
        Expr::BinOp(lh, op, rh) => {
            let lh_type = get_type(lh, &constraints)?;
            let rh_type = get_type(rh, &constraints)?;
            if lh_type != rh_type {
                return Err(TypeError::Mismatch(lh_type, rh_type));
            }
            match op {
                Op::Add | Op::Min => match lh_type {
                    Type::Primitive(Primitive::Float) | 
                    Type::Primitive(Primitive::Int) |
                    Type::Primitive(Primitive::List(_)) => Ok(lh_type),
                    _ => Err(TypeError::UnsupportedOp(lh_type, *op, rh_type)),
                }
                Op::Eq => Ok(Type::Primitive(Primitive::Bool)),
                Op::Gt | Op::Lt => match lh_type {
                    Type::Primitive(Primitive::Float) | 
                    Type::Primitive(Primitive::Int) => Ok(Type::Primitive(Primitive::Bool)),
                    _ => Err(TypeError::UnsupportedOp(lh_type, *op, rh_type)),
                }
            }
        }
        Expr::Ident(ident) => constraints.get(&ident).cloned().ok_or(TypeError::MissingIdent(ident.clone())),
        Expr::Call(_, _) => unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::{Primitive, Type, get_type};
    use parse::Lit::*;
    use parse::Expr::{BinOp, Lit, Ident};
    use parse::{Expr, Op, Ident as Identifier};
    use std::collections::HashMap;

    #[test]
    fn int_list() {
        let e = Lit(List(vec![Lit(Int(1)), Lit(Int(2)), Lit(Int(3))]));
        let ty = get_type(&e, &HashMap::default()).unwrap();
        assert_eq!(ty, Type::Primitive(Primitive::List(Box::new(Type::Primitive(Primitive::Int)))));
    }

    #[test]
    fn list_empty() {
        let e = Lit(List(vec![]));
        let ty = get_type(&e, &HashMap::default()).unwrap();
        assert_eq!(ty, Type::Primitive(Primitive::List(Box::new(Type::Free))));
    }

    #[test]
    #[should_panic]
    fn list_mismatch() {
        let e = Lit(List(vec![Lit(Int(1)), Lit(Float(1.0))]));
        let _ty = get_type(&e, &HashMap::default()).unwrap();
    }

    #[test]
    fn add_floats() {
        let e = BinOp(
            Box::new(Lit(Float(1.0))),
            Op::Add,
            Box::new(Lit(Float(2.0)))
        );
        let ty = get_type(&e, &HashMap::default()).unwrap();
        assert_eq!(ty, Type::Primitive(Primitive::Float));
    }

    #[test]
    fn string_equality() {
        let e = BinOp(
            Box::new(Lit(Str("hello".to_string()))),
            Op::Eq,
            Box::new(Lit(Str("world".to_string())))
        );
        let ty = get_type(&e, &HashMap::default()).unwrap();
        assert_eq!(ty, Type::Primitive(Primitive::Bool));
    }

    #[test]
    #[should_panic]
    fn string_compare() {
        let e = BinOp(
            Box::new(Lit(Str("hello".to_string()))),
            Op::Gt,
            Box::new(Lit(Str("world".to_string())))
        );
        let _ty = get_type(&e, &HashMap::default()).unwrap();
    }

    #[test]
    fn add_ident() {
        let ident = Identifier("a".to_string());
        let e = BinOp(
            Box::new(Lit(Int(1))),
            Op::Add,
            Box::new(Expr::Ident(ident.clone()))
        );
        let mut constraints = HashMap::new();
        constraints.insert(ident, Type::Primitive(Primitive::Int));
        let ty = get_type(&e, &constraints).unwrap();
        assert_eq!(ty, Type::Primitive(Primitive::Int));
    }
}
