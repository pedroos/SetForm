namespace Pslab.Limits.SetForm

module Oo = 
    // 1. 
    // Object-oriented form of Sets with Set elements.

    type Oset<'t> (elems: List<Oset<'t>>) = 
        member this.elems = elems
        
    // SetElement as a subtype of Set with a single value and no Sets which 
    // initializes an empty Set only for the subtyping; not optimal OO form. 
    // Like an Urelement.

    type SetElement<'t> (x: option<'t>) = 
        inherit Oset<'t>([])
        member this.x = x
    
    // Alternative construction. Assignment of the Set Element as a Set object 
    // with better OO form. However, it recurses infinitely at runtime.

    type SetElement2<'t> (x: option<'t>) = 
        inherit Oset<'t>([SetElement2<'t>(x)])
        member this.x = x
        
    // OO Tuples as Sets. Arities needs individual definitions.
    
    type Otuple2<'t>(a: Oset<'t>, b: Oset<'t>) = 
        inherit Oset<'t>([a; Oset<'t>([a; b])])
        member this.a = a
        member this.b = b
            
    type Otuple3<'t>(a: Oset<'t>, b: Oset<'t>, c: Oset<'t>) = 
        inherit Otuple2<'t>(Otuple2<'t>(a, b), c)
                    
    type Otuple4<'t>(a: Oset<'t>, b: Oset<'t>, c: Oset<'t>, d: Oset<'t>) = 
        inherit Otuple2<'t>(Otuple2<'t>(Otuple2<'t>(a, b), c), d)

    let construct() = 
        // Non-Element Osets must be formed of Elements, in turn formed of 
        // values

        Oset<int>([
            SetElement<int>(Some(0)); 
            SetElement<int>(Some(1))
        ]) |> ignore

        // An Osets's element entails an Element element
        
        let s = 
            Oset<int>([
                Oset<int>([
                    SetElement<int>(Some(0))
                ]); 
                Oset<int>([
                    SetElement<int>(Some(1))
                ]); 
                SetElement<int>(Some(2))
            ])
        
        // Recurses infinitely

        //let s = 
        //    Oset<int>([
        //        Oset<int>([
        //            SetElement2<int>(Some(0))
        //        ]); 
        //        Oset<int>([
        //            SetElement2<int>(Some(1))
        //        ]); 
        //        SetElement2<int>(Some(2))
        //    ])

        // An Oset allows multiple empty Elements at the same level

        Oset<int>([
            SetElement<int>(None); 
            SetElement<int>(None)
        ]) |> ignore
            
        ()

    let print() = 
        ()