use std::{cell::RefCell, ops::{Index, IndexMut}, rc::Rc};

use crate::{idx, translate::member::{TranslationCtxt}, vmir::{middle::{ExpLine, ExpLineKind, ExpOperand}, ty::{Interner, ParamTy, Ty, TyKind, TypeFolder}, Symbol}, HashSet, TiVec};

static IFX_NAME: Symbol<'static> = Symbol::new_static("@param_ifx");

idx!(InferIdx, "ifx{}");

#[derive(Debug, Default, Clone)]
pub struct TypeChecker<'tcx>(Rc<RefCell<TypeCheckerInner<'tcx>>>);

impl<'tcx> TypeChecker<'tcx> {
    fn inner(&mut self) -> std::cell::RefMut<'_, TypeCheckerInner<'tcx>> {
        self.0.borrow_mut()
    }

    pub(crate) fn any_ty<'a>(&mut self, tcx: &'a TranslationCtxt<'_, 'tcx>) -> TyIfx<'a, 'tcx> {
        let ty = self.inner().any_ty(tcx);
        TyIfx(ty, self.clone(), &tcx.tcx.interner)
    }

    pub(crate) fn any_address<'a>(&mut self, tcx: &'a TranslationCtxt<'_, 'tcx>) -> TyIfx<'a, 'tcx> {
        let ty = self.inner().any_address(tcx);
        TyIfx(ty, self.clone(), &tcx.tcx.interner)
    }

    pub(crate) fn constrain(&mut self, ty: Ty<'tcx>, t: ExpectedTys<'tcx>) {
        self.inner().constrain(ty, t);
    }
}

#[derive(Debug, Default, Clone)]
struct TypeCheckerInner<'tcx> {
    infer_tys: TiVec<InferIdx, TypeInfer<'tcx>>,
}

impl<'tcx> TypeCheckerInner<'tcx> {
    fn any_ty(&mut self, tcx: &TranslationCtxt<'_, 'tcx>) -> Ty<'tcx> {
        let index = self.infer_tys.push_and_get_key(TypeInfer::new(None));
        println!("[A] mk {index:?}");
        tcx.tcx.interner.mk_ty_from_kind(TyKind::Param(ParamTy {
            name: IFX_NAME,
            index: usize::from(index) as u32,
        }))
    }

    fn any_address(&mut self, tcx: &TranslationCtxt<'_, 'tcx>) -> Ty<'tcx> {
        let inner = self.any_ty(tcx);
        tcx.tcx.interner.mk_ty_from_kind(TyKind::Address(inner))
    }

    fn constrain(&mut self, ty: Ty<'tcx>, expected: ExpectedTys<'tcx>) {
        match expected {
            ExpectedTys::Any => (),
            ExpectedTys::Exact(other) => self.equate(ty, other),
            ExpectedTys::AnyPrim(expected) => {
                let Some(p) = ty.as_ifx() else {
                    assert!(expected.contains(&ty), "expected primitive type (any of {expected:?}), found {ty:?}");
                    return;
                };
                let TypeInferState::Constrained(other) = &mut self[p].state else {
                    unreachable!()
                };
                match other {
                    ExpectedTys::Any => *other = ExpectedTys::AnyPrim(expected),
                    ExpectedTys::AnyPrim(other) => {
                        let new = other.intersection(&expected).copied().collect::<HashSet<Ty<'tcx>>>();
                        assert!(!new.is_empty(), "type constrained as both {other:?} and {expected:?}; no possible types");
                        *other = new;
                    }
                    ExpectedTys::Exact(ty) =>
                        assert!(expected.contains(ty), "type constrained as {ty:?}, but expected any of {expected:?}"),
                }
            }
        }
    }

    fn equate(&mut self, a: Ty<'tcx>, b: Ty<'tcx>) {
        if a == b {
            return;
        }
        match (a.as_ifx(), b.as_ifx()) {
            (None, None) => (),
            (Some(a), None) => return self.constrain_ifx(a, b),
            (None, Some(b)) => return self.constrain_ifx(b, a),
            (Some(ai), Some(bi)) => {
                let ((si, st), (bi, _bt)) = if ai <= bi {
                    ((ai, a), (bi, b))
                } else {
                    ((bi, b), (ai, a))
                };
                let expected = core::mem::replace(&mut self[bi].state, TypeInferState::Equal(si));
                let TypeInferState::Constrained(expected) = expected else {
                    unreachable!();
                };
                return self.constrain(st, expected);
            }
        }
        match (a.kind(), b.kind()) {
            (TyKind::Address(a), TyKind::Address(b)) => {
                self.equate(*a, *b);
            }
            (TyKind::Domain(dk_a, args_a), TyKind::Domain(dk_b, args_b)) => {
                assert_eq!(dk_a, dk_b);
                for (a, b) in args_a.iter().zip(args_b.iter()) {
                    self.equate(*a, *b);
                }
            }
            _ => panic!("type error: expected {a:?}, found {b:?}"),
        }
    }

    fn constrain_ifx(&mut self, a: InferIdx, b: Ty<'tcx>) {
        assert!(b.as_ifx().is_none());
        let c = match &mut self[a].state {
            TypeInferState::Resolving | TypeInferState::Equal(..) => unreachable!(),
            TypeInferState::Constrained(c) => c,
            TypeInferState::Resolved(ty) => {
                let a = *ty;
                return self.equate(a, b)
            }
        };
        match c {
            ExpectedTys::Any => (),
            ExpectedTys::Exact(ty) => {
                let a = *ty;
                return self.equate(a, b)
            }
            ExpectedTys::AnyPrim(tys) => assert!(tys.contains(&b), "type error: expected one of {tys:?}, found {b:?}"),
        };
        *c = ExpectedTys::Exact(b);
    }

    fn resolve_ty(&mut self, interner: &Interner<'tcx>, ty: Ty<'tcx>) {
        let mut folder = ParamTypeResolver {
            infcx: self,
            interner,
        };
        folder.fold_ty(ty);
    }

    fn final_idx(&self, mut index: InferIdx) -> InferIdx {
        loop {
            match self.infer_tys[index].state {
                TypeInferState::Equal(other) => index = other,
                TypeInferState::Constrained(ExpectedTys::Exact(ty)) => {
                    let Some(other) = ty.as_ifx() else {
                        break;
                    };
                    index = other;
                }
                _ => break,
            }
        }
        index
    }
}

impl<'tcx> Index<InferIdx> for TypeCheckerInner<'tcx> {
    type Output = TypeInfer<'tcx>;

    fn index(&self, index: InferIdx) -> &Self::Output {
        let index = self.final_idx(index);
        &self.infer_tys[index]
    }
}

impl<'tcx> IndexMut<InferIdx> for TypeCheckerInner<'tcx> {
    fn index_mut(&mut self, mut index: InferIdx) -> &mut Self::Output {
        let f_index = self.final_idx(index);
        loop {
            match *&mut self.infer_tys[index].state {
                TypeInferState::Equal(ref mut other) =>
                    index = core::mem::replace(other, f_index),
                ref mut state@TypeInferState::Constrained(ExpectedTys::Exact(ty)) => {
                    let Some(other) = ty.as_ifx() else {
                        break;
                    };
                    *state = TypeInferState::Equal(f_index);
                    index = other;
                }
                _ => break,
            }
        }
        &mut self.infer_tys[f_index]
    }
}

pub struct TyIfx<'a, 'tcx>(Ty<'tcx>, TypeChecker<'tcx>, &'a Interner<'tcx>);

impl<'tcx> Drop for TyIfx<'_, 'tcx> {
    fn drop(&mut self) {
        self.1.inner().resolve_ty(self.2, self.0);
    }
}

impl<'tcx> TyIfx<'_, 'tcx> {
    pub fn ty(&self) -> Ty<'tcx> {
        self.0
    }
}

impl<'tcx> Ty<'tcx> {
    fn as_ifx(self) -> Option<InferIdx> {
        if let TyKind::Param(p) = *self.kind() {
            if p.name == IFX_NAME {
                return Some(InferIdx::from(p.index as usize));
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
struct TypeInfer<'tcx> {
    /// The param type this originates from (e.g. when typechecking an
    /// `adt Option[T]` and expression `Some(42)`, this is the `T` of an
    /// `Option[Infer]` assigned initially to the expression).
    param: Option<ParamTy<'tcx>>,
    state: TypeInferState<'tcx>,
}

impl<'tcx> TypeInfer<'tcx> {
    fn new(param: Option<ParamTy<'tcx>>) -> Self {
        Self {
            param,
            state: TypeInferState::Constrained(ExpectedTys::Any),
        }
    }
}

#[derive(Debug, Clone)]
enum TypeInferState<'tcx> {
    Constrained(ExpectedTys<'tcx>),
    Equal(InferIdx),
    Resolving,
    Resolved(Ty<'tcx>),
}

#[derive(Debug, Default, Clone)]
pub enum ExpectedTys<'tcx> {
    #[default]
    Any,
    Exact(Ty<'tcx>),
    AnyPrim(HashSet<Ty<'tcx>>),
}

impl<'tcx> From<Option<Ty<'tcx>>> for ExpectedTys<'tcx> {
    fn from(ty: Option<Ty<'tcx>>) -> Self {
        match ty {
            Some(ty) => Self::Exact(ty),
            None => Self::Any,
        }
    }
}

impl<'tcx> From<&[Ty<'tcx>]> for ExpectedTys<'tcx> {
    fn from(tys: &[Ty<'tcx>]) -> Self {
        assert!(tys.iter().all(|ty| ty.is_primitive()), "expected tys must be primitive");
        Self::AnyPrim(tys.iter().copied().collect())
    }
}

// impl<'tcx> ExpectedTys<'tcx> {
//     fn any_prim(&self) -> Option<&HashSet<Ty<'tcx>>> {
//         match self {
//             Self::AnyPrim(tys) => Some(tys),
//             _ => None,
//         }
//     }

//     fn want_real(&self) -> bool {
//         self.any_prim().is_some_and(|tys| tys.len() == 1 && matches!(tys.first().unwrap().kind(), TyKind::Real))
//     }
// }














// #[derive(Debug, Default, Clone)]
// pub enum ExpectedTys<'tcx> {
//     #[default]
//     Any,
//     Adt(AdtId),
//     Exact(Ty<'tcx>),
//     AnyPrim(HashSet<Ty<'tcx>>),
// }

// impl<'tcx, I: IntoIterator<Item = Ty<'tcx>>> From<I> for ExpectedTys<'tcx> {
//     fn from(tys: I) -> Self {
//         let mut all_primitive = true;
//         let tys: HashSet<_> = tys.into_iter().map(|ty| {
//             all_primitive &= ty.is_primitive();
//             ty
//         }).collect();
//         if all_primitive {
//             let tys = Some(tys).filter(|ty| !ty.is_empty());
//             tys.map_or(Self::Any, Self::AnyPrim)
//         } else {
//             assert_eq!(tys.len(), 1, "expected tys with params must have exactly one type");
//             Self::Exact(*tys.iter().next().unwrap())
//         }
//     }
// }

// impl<'tcx> ExpectedTys<'tcx> {
//     fn any_prim(&self) -> Option<&HashSet<Ty<'tcx>>> {
//         match self {
//             Self::AnyPrim(tys) => Some(tys),
//             _ => None,
//         }
//     }

//     fn want_real(&self) -> bool {
//         self.any_prim().is_some_and(|tys| tys.len() == 1 && matches!(tys.first().unwrap().kind(), TyKind::Real))
//     }
// }

// #[derive(Debug, Clone, Copy)]
// struct TypeParamPossible<'tcx> {
//     infer_param: ParamTy<'tcx>,
//     possible: TypeParamResolutionState<'tcx>,
// }

// #[derive(Debug, Clone, Copy)]
// enum TypeParamResolutionState<'tcx> {
//     Unconstrained,
//     Constrained(Ty<'tcx>),
//     Resolving,
//     Resolved(Ty<'tcx>),
// }

// impl<'tcx> TypeParamPossible<'tcx> {
//     fn new(ty: ParamTy<'tcx>) -> Self {
//         Self {
//             infer_param: ty,
//             possible: TypeParamResolutionState::Unconstrained,
//         }
//     }
// }

// #[derive(Debug, Default, Clone)]
// pub struct ParamInfcx<'tcx> {
//     params: Vec<TypeParamPossible<'tcx>>,
// }

// impl<'tcx> ParamInfcx<'tcx> {
//     fn check_ty(&mut self, e: &ExpectedTys<'tcx>, ty: Ty<'tcx>) {
//         match e {
//             ExpectedTys::Any => (),
//             ExpectedTys::Adt(adt) => {
//                 if matches!(ty.kind(), TyKind::Domain(DomainKind::Adt(a), _) if a == adt) {
//                     return;
//                 }
//                 panic!("type error: expected adt {adt:?}, found {ty:?}");
//             }
//             ExpectedTys::Exact(e) => {
//                 self.assert_tys_match(*e, ty)
//             }
//             ExpectedTys::AnyPrim(tys) => {
//                 assert!(ty.is_primitive(), "type error: expected primitive type, found {ty:?}");
//                 assert!(tys.contains(&ty), "type error: expected one of {tys:?}, found {ty:?}");
//             }
//         }
//     }

//     fn assert_tys_match(&mut self, e: Ty<'tcx>, ty: Ty<'tcx>) {
//         if e == ty {
//             return;
//         }
//         match (e.kind(), ty.kind()) {
//             (TyKind::Param(p), _) | (_, TyKind::Param(p)) => {
//                 let swap = ty.param().is_some_and(|ty| ty.index > p.index);
//                 if swap {
//                     self.update_param(ty.param().unwrap().index as usize, e)
//                 } else {
//                     self.update_param(p.index as usize, ty)
//                 };
//             }
//             (TyKind::Domain(ek, etys), TyKind::Domain(k, tys)) => {
//                 assert_eq!(ek, k, "type error: expected domain {ek:?}, found {k:?}");
//                 assert_eq!(etys.len(), tys.len(), "internal error");
//                 for (e, ty) in etys.iter().zip(tys.iter()) {
//                     self.assert_tys_match(*e, *ty);
//                 }
//             }
//             (TyKind::Address(e), TyKind::Address(ty)) => self.assert_tys_match(*e, *ty),
//             (a, b) => {
//                 assert_eq!(core::mem::discriminant(e.kind()), core::mem::discriminant(ty.kind()), "type error: expected {a:?}, found {b:?}");
//                 assert_eq!(a, b, "internal error")
//             }
//         };
//     }

//     fn update_param(&mut self, idx: usize, ty: Ty<'tcx>) {
//         println!("[C] {idx} := {ty}");
//         let possible = &mut self.params[idx].possible;
//         use TypeParamResolutionState::*;
//         let e = match *possible {
//             Unconstrained => {
//                 *possible = Constrained(ty);
//                 return;
//             }
//             Constrained(e) => e,
//             Resolving => unreachable!("{:?}", possible),
//             Resolved(e) => {
//                 if let TyKind::Param(p) = ty.kind() {
//                     let p = self.params[p.index as usize].possible;
//                     assert!(matches!(p, Resolved(p) if p == e), "internal error: expected {e:?}, found {p:?}");
//                 } else {
//                     assert_eq!(e, ty);
//                 }
//                 return;
//             }
//         };
//         self.assert_tys_match(e, ty);
//     }

//     fn mk_param_list(&mut self, tcx: &TranslationCtxt<'_, 'tcx>, ps: &[ParamTy<'tcx>]) -> TyList<'tcx> {
//         self.params.reserve(ps.len());
//         let tys = ps.iter().map(|p| {
//             self.params.push(TypeParamPossible::new(*p));
//             tcx.tcx.interner.mk_ty_from_kind(TyKind::Param(ParamTy {
//                 name: Symbol::new_static("@param_infcx"),
//                 index: self.params.len() as u32 - 1,
//             }))
//         }).collect::<Vec<_>>();
//         println!("[A] [{}] -> [{}]",
//             ps.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", "), 
//             tys.iter().map(|t| t.param().unwrap().to_string()).collect::<Vec<_>>().join(", ")
//         );
//         tcx.tcx.interner.mk_ty_list(tys)
//     }

//     fn resolve_ty(&mut self, tcx: &TranslationCtxt<'_, 'tcx>, ty: &mut Ty<'tcx>) {
//         let mut folder = ParamTypeResolver {
//             infcx: self,
//             interner: &tcx.tcx.interner,
//         };
//         *ty = folder.fold_ty(*ty)
//     }

//     fn any_ty(&mut self, tcx: &TranslationCtxt<'_, 'tcx>) -> Ty<'tcx> {
//         self.params.push(TypeParamPossible::new(ParamTy {
//             name: Symbol::new_static("any_wildcard"),
//             index: u32::MAX,
//         }));
//         println!("[A] any_wildcard -> {}", self.params.len() - 1);
//         tcx.tcx.interner.mk_ty_from_kind(TyKind::Param(ParamTy {
//             name: Symbol::new_static("@param_infcx"),
//             index: self.params.len() as u32 - 1,
//         }))
//     }

//     pub(super) fn any_address(&mut self, tcx: &TranslationCtxt<'_, 'tcx>) -> Ty<'tcx> {
//         let inner = self.any_ty(tcx);
//         tcx.tcx.interner.mk_ty_from_kind(TyKind::Address(inner))
//     }
// }

struct ParamTypeResolver<'a, 'tcx> {
    infcx: &'a mut TypeCheckerInner<'tcx>,
    interner: &'a Interner<'tcx>,
}

impl<'tcx> TypeFolder<'tcx> for ParamTypeResolver<'_, 'tcx> {
    fn interner(&self) -> &Interner<'tcx> {
        self.interner
    }

    fn inner_fold_ty(&mut self, ty: Ty<'tcx>) -> Option<Ty<'tcx>> {
        if let TyKind::Param(p) = *ty.kind() {
            let idx = InferIdx::from(p.index as usize);
            let p = &mut self.infcx.infer_tys[idx];
            use TypeInferState::*;
            let ty = match &mut p.state {
                Constrained(ExpectedTys::Any) =>
                    panic!("type parameter {:?} is unconstrained and thus cannot be inferred", p.param),
                Constrained(ExpectedTys::AnyPrim(prims)) => {
                    assert_eq!(prims.len(), 1, "Expected exactly one primitive type");
                    return Some(*prims.iter().next().unwrap());
                }
                state@&mut Constrained(ExpectedTys::Exact(ty)) => {
                    *state = Resolving;
                    ty
                }
                Equal(eq) => {
                    todo!()
                }
                Resolving => panic!("type parameter {:?} is already being resolved", p.param),
                Resolved(ty) => return Some(*ty),
            };
            let ty = self.fold_ty(ty);
            println!("- [R] {:?} -> {}", idx, ty);
            self.infcx.infer_tys[idx].state = Resolved(ty);
            Some(ty)
        } else {
            Some(self.super_fold_ty(ty))
        }
    }
}
