/* 
`ded-nat` is a function that expects 2 parameters:
- `stcolor`: the stroke color of the indentation guides. The default is `black`.
- `arr`: an array with the shape, it can be provided in two shapes.
    - 4 items: (dependency: text content, indentation: integer starting from 0, formula: text content, rule: text content).
    - 3 items: the same as above, but without the dependency.
    Additionally, for the 4 & 3 item variants, it is possible to insert "comment lines", of just 1 text item like the following example:
      - `#ded-nat(arr: ((0, $P -> Q$, "PR"), ("After the premise"), (0, $P$, "PR"))
*/
#let ded-nat(stcolor: black,  arr: array) = context {
  let strart = ( top: 0em, right: 0em, bottom: 0em,left: 1pt + stcolor  )
  let strend = ( top: 0em, right: 0em, bottom: 1pt + stcolor, left: 1pt + stcolor )

  // ((type: array/string, # of lines of), ...)
  let countArray = ()
  let prevType = none
  let numLinesPrev = 0
  for line in arr {
    let currType = type(line)
    if currType == prevType {
      numLinesPrev += 1
    } else {
      if prevType != none {countArray.push((prevType, numLinesPrev))}
      prevType = currType
      numLinesPrev = 1
    }
  }
  if prevType != none {countArray.push((prevType, numLinesPrev))}

  let commArray = arr.filter(it => type(it) == str).map(it => box(inset:(x:0em, y:0.5em), it))

  let formArr = arr.filter(it => type(it) != str)
  // check if the first line's array has 4 items (w/dependencies) or not
  let hasDependencies = if formArr.at(0).len() == 4 {true} else {false}

  let maxDepWidth = if hasDependencies {
      formArr.fold(0pt, (accum, it) => {
      let sz = measure[#it.at(0)].width
      if sz > accum {sz} else {accum}  
    } )
  } else {
    0pt
  }
  
  let maxWidth = 0pt
  let tupArr = ()
  
  for (i, line) in formArr.enumerate(start: 0) {
    let (dep, indent, formula, rule) = if hasDependencies {
      line
    } else {
      ("", line.at(0), line.at(1), line.at(2))
    }

    let isLastIndented = (formArr.len() > i+1) and ({
      let ix = if hasDependencies {1} else {0}
        formArr.at(i+1).at(ix)
      } < indent)

    let bl = formula
    
    for i in range(0, indent){
      let inset = if i != 0 {0pt} else {0pt}

      let str = if i == 0 and isLastIndented {
        strend 
      } else{ strart  }

      if indent > 0 and i < indent - 1{
        inset = -5pt
      } 
      
      bl = [#h(1em) #box(inset: inset)[#rect(stroke: str)[#bl]]]
    }
    let index = box(width: 1.5em, text(weight: "bold")[#(i+1).])

    let tmpWidth = 0pt
    let dependency = ""
    if hasDependencies {
      dependency = text(style: "italic", weight: "regular", box(width: maxDepWidth + 1em)[#dep])
      tmpWidth = measure[#dependency #index #bl].width
    } else {
      tmpWidth = measure[#index #bl].width
    }

    if tmpWidth > maxWidth {
      maxWidth = tmpWidth
    }

    let ins = if indent == 0 {(x: 0pt, y: 0.5em)} else {(x: 0pt, y: 0pt)}
    let numInset = if indent == 0 {(x: 0pt, y: 0.0pt)} else {(x: 0pt, y: 5pt)}

    let line = ""
    if hasDependencies {
      line = box(inset: ins)[#box(inset: numInset)[#dependency #index] #bl]
    } else {
      line = box(inset: ins)[#box(inset: numInset)[#index] #bl]
    }
    
    rule = box(baseline: -0.5em , text(weight: "bold", rule))
    tupArr.push((line, rule))
  }


  tupArr = tupArr.map(a => [#box(width: maxWidth + 0em, a.at(0)) #h(1em) #a.at(1)])

  let assembled = ()
  let str_index = 0
  let form_index = 0
  for (type_name, count) in countArray{
    if type_name == str{
      assembled = (assembled, commArray.slice(str_index, count:count)).flatten()
      str_index += count
    }
    else if type_name == array {
      assembled = (assembled, tupArr.slice(form_index, count:count)).flatten()
      form_index += count
    }
  }

  block(
    align(start,
      stack(dir: ttb, spacing: 0em, ..assembled)
    )
  )
}

/*
`ded-nat-boxed` is a function that expects 4 parameters, and returns the deduction in a `box`:
- `stcolor`: the stroke color of the indentation guides. The default is `black`.
- `premises-and-conclusion`: bool, whether to automatically insert or not the premises and conclusion of the derivation above the lines. The default is `true`.
- `premise-rule-text`: text content, used for finding the premises' formulas when `premises-and-conclusion` is set to `true`. The default is `"PR"`.
- `arr`: an array with the shape, it can be provided in two shapes.
    - 4 items: (dependency: text content, indentation: integer starting from 0, formula: text content, rule: text content).
    - 3 items: the same as above, but without the dependency.
    Additionally, for the 4 & 3 item variants, it is possible to insert "comment lines", of just 1 text item like the following example:
      - `#ded-nat-boxed(arr: ((0, $P -> Q$, "PR"), ("After the premise"), (0, $P$, "PR"))
*/
#let ded-nat-boxed(stcolor: black, premises-and-conclusion: true, premise-rule-text: "PR", arr: array) ={
  let premConcText = ""
  if premises-and-conclusion {
    let premises = arr.filter(x => type(x) == array).filter( x => x.last() == premise-rule-text).map(x => x.at(2))
    let conclusion = arr.filter(x => type(x) == array).last().at(2)
    let joinedPremises = [#premises.join([$, $ \ ] )]
    premConcText = [ $\ #joinedPremises \ tack #conclusion$ ]  
  }
  
  box(
    stroke: stcolor, inset: 8pt, radius: 8pt
  )[
   #align(center)[
      #if premises-and-conclusion {
        premConcText
      } #ded-nat(stcolor: stcolor, arr: arr)
    ]
  ]
}

