namespace Pslab.Limits.SetForm

module MixedRename = 
    // 3b.
    // A rename of 3).

    type Set<'t> (elems: List<Rec<'t>>) = 
        member this.elems = elems
          
    and SetElement<'t> (x: option<'t>) = 
        inherit Set<'t>([])
        member this.x = x

    and Rec<'t> = Su of Set<'t> | Te of option<'t>

    // For Pair Set, use form {a,b} here instead of {{a},{a,b}} for traversal 
    // as Sets.
        
    type Pair<'t>(a: Rec<'t>, b: Rec<'t>) = 
        inherit Set<'t>([
            a; 
            b
        ])
        member this.a = a
        member this.b = b
        
    type Tuple3<'t>(a: Rec<'t>, b: Rec<'t>, c: Rec<'t>) = 
        inherit Pair<'t>(
            Su(Pair<'t>(
                a, 
                b
            )), 
            c
        )
                    
    and Tuple4<'t>(a: Rec<'t>, b: Rec<'t>, c: Rec<'t>, d: Rec<'t>) = 
        inherit Pair<'t>(
            Su(Pair<'t>(
                Su(Pair<'t>(
                    a, 
                    b
                )), 
                c
            )), 
            d
        )

    // Try to avoid a runtime cast from Pair backwards to Set, i.e. when 
    // traversing over Pairs as Pairs. Abstracts the recursive component.

    type Uset<'t> (elems: List<Urec<Uset<'t>, 't>>) = 
        member this.elems = elems

    and Urec<'su, 't> = Usu of 'su | Ute of option<'t> 
    
    // .. but it doesn't work because of variance issues which mean Urec<Uset> 
    // is not a supertype of Urec<Pair>. OCaml could support it.

    //type Upair<'t>(a: Urec<Upair<'t>, 't>, b: Urec<Upair<'t>, 't>) = 
    //    inherit Uset<'t>([
    //        a :> Urec<Uset<'t>, 't>; 
    //        b :> Urec<Uset<'t>, 't>
    //    ])
    //    member this.a = a
    //    member this.b = b

    let construct() =         
        Su(Set<int>([
            Te(None); 
            Te(Some(1))
        ])) |> ignore
        
        Su(Set<int>([
            Su(SetElement(None)); 
            Su(SetElement(Some(1)))
        ])) |> ignore
        
        Su(Set<int>([
            Su(SetElement(None)); 
            Te(Some(1))
        ])) |> ignore

        Su(Set<int>([
            Su(SetElement(None)); 
            Su(Set<int>([
                Su(SetElement(Some(1)))
            ]))
        ])) |> ignore
        
        Su(Set<int>([
            Te(None); 
            Su(Set<int>([
                Te(None)
            ]))
        ])) |> ignore

        Set<int>([
            Te(None)
        ]) |> ignore

        Pair<int>(
            Te(None), 
            Te(None)
        ) |> ignore
        
        Su(Pair<int>(
            Te(None), 
            Te(None)
        )) |> ignore
        
        Su(Pair<int>(
            Su(Set<int>([Te(None)])), 
            Te(None)
        )) |> ignore
        
        Su(Pair<int>(
            Su(Pair<int>(
                Te(None), 
                Su(Set<int>([Te(None)]))
            )), 
            Te(None)
        )) |> ignore

        let _: Rec<int> = 
            Su(Pair<int>(
                Su(Pair<int>(
                    Te(Some(1)), 
                    Te(Some(2))
                )), 
                Su(Pair<int>(
                    Su(Pair<int>(
                        Te(Some(3)), 
                        Te(Some(4))
                    )), 
                    Te(Some(5))
                ))
            ))

        ()

    let print() = 
        ()