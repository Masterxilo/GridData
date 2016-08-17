<<PackageDeveloper`

BeginPackage["GridData`", {"FiniteMapping`", "paul`", "numerics`", "PackageDeveloper`"}]

ClearAll["GridData`*", "GridData`Private`*"]

PublicSymbols[
  GridDataMakeFromArray
  ,GridData
  ,GridDataMakeFromImage
  ,GridDataMakeFromImage3D
  ,GridDataMakeFromArrays
  ,GridDataMakeFromRules
  ,GridDataMakeFromFiniteMapping
  ,GDToArray
  ,GDSingleDatumToArray
  ,GDToImage
  ,GDToImage3D
  ,GDAsRules
  ,GDAsRulesOfRules
  ,GDAsPairedRules
  ,GDAsPairedAtomRules
  ,GDAsArraysRule
  ,GDUpdate
  ,GDArrayDepth
  ,GDToNoninterleavingArray
  ,GDToArrays
  ,GDDomain
  ,GDCoordinateBounds
  ,GDCoordinateBoundingBox
  ,GDDataNames
  ,GDCoordinateQ
  ,GDLookup
  ,GDLookupList
  ,GDSubset
  ,GDSubsetCoordinateBounds
  ,GDDataNamesLength
  ,GDToFiniteMapping
  ,GDToNestedFiniteMapping
  ,GDMap
  ,GDMapList
  ,GDDataSubset
  ,GDMapPositions
  ,GDPrependDimension
  ,GDRestDimension
  ,GDAppendDimension
  ,GDMostDimension
  ,GDSelect
  ,GDSelectList
  ,GDEdgeFilter
  ,GDErode
  ,GDCopy
  ,GDNearestFilter
  ]
(* -- Purpose -- *)
(*
Store immutable data associated with positions on a uniform grid.

Abstracts away the details of storing a fixed data-structure for each point in
an n-dimensional (sparse) grid.

Allows converting between various representations for such data,
as well as extracting portions of data as an array of structures,
a structure of arrays and mixtures of these.

Can efficiently access any elements and rectangular subset of elements
in the grid, as well as do re-indexing.

This can be considered a special kind of FiniteMapping, Array, Image, Image3D, SparseArray,
hash-indexed collection (vector, list), expression used to store data (by positions) etc.
*)

(* TODO *)
(*
- support empty?
- support copying some attributes to others (generalize DataSubset)
- support copying/moving regions around
- support densifying
- detect and report repeated variable names and other errors
- detect when the grid is dense
- allow efficient dense indexing of small neighborhoods (e.g. for objective functions in optimizations, convolutions, filtering etc.)
- treat the case of NumericVector valued data (i.e. a list of floats) specially
 ^ consider implementing it as if this where the case even for differently nested lists (
 e.g. data of the form { {nx, nx}, d, {r,g,b}} or even with a matrix
- investigate how Association and or Dataset perform with these tasks
*)

(* -- Purpose -- *)
(*
Store immutable data associated with positions on a uniform grid.
*)

(* Implementation notes *)
(* Valid forms are:
GridData[dataNames_List, f_FiniteMapping]
 f maps coordinates to a list of values of length
 dataNames, which are associated with dataNames in their given order

 Conceptually, we have a FinteMapping that creates a finite mapping at each point.
 We just don't store the keys (that are shared!) on each inner finite mapping.

 TODO use custom structure to optimize expected queries
 TODO store more data to correct user errors
 - maybe rely on internal structure of FiniteMapping (to be Array or SparseArray)
 - allow querying whether it is dense?
 - specify a pattern on the data (all reals, more grids etc.)
 - convert to and from Array, Image, Image3D
 - allow filtering to sparsify (ala SelectQ/DeleteCases) and to
   extend -> NearestFilter

 - generate from image which is discretized distance function to mesh (-> volumetric
   representation of a mesh).

TODO allow more operations on data (map)
TODO allow accessing points and neightborhoods
TODO allow applying filters (convolutions and non-uniform/linear, e.g. bilateral, hole filling (dilate)) operating on neightborhoods
TODO develop syntax package that abstracts away the internal structure (Format rules), allows more concise code,

TODO distribute
Todo convert to and from image

(* TODO support non-numeric data *)
TODO note that SparseArray can store non-numeric data
TODO deal with non list (single item) data
*)

Begin@"`Private`";

(* Name *)
GridData

(* Attributes *)
GridData~SetAttributes~HoldAllComplete



















DefinePublicFunction[

  GridDataMakeFromArray[
    dataNames_List
    , array_
    , level_Integer
  ]

  /;

      IsArrayAtLevel[array, level] &&
          Dimensions[array][[level+1]] === Length@dataNames

  , "Make a dense grid of data"

  , {f=FiniteMappingMakeFromArray[array, level]}~With~GridData[dataNames, f]

];





















DefinePublicFunction[
  GridDataMakeFromArray[dataNames_List, array_?ArrayQ]
  ,"automatically infer the level"
  ,GridDataMakeFromArray[dataNames, array, ArrayDepth@array-1]
];

DefinePublicFunction[
  GridDataMakeFromArray[array_, level_Integer] /; IsArrayAtLevel[array, level]
  ,"auto-naming"
  ,Module[{dataNames},
    dataNames = FMMakeListDomainNames[ Dimensions[array][[level+1]] ];
    GridDataMakeFromArray[dataNames, array, level]
  ]
];

DefinePublicFunction[
  GridDataMakeFromArray[array_?ArrayQ]
  ,"auto-naming and level"
  ,GridDataMakeFromArray[array, ArrayDepth@array-1]
];

DefinePublicFunction[
  GridDataMakeFromImage[i : (_Image | _Image3D)] ,
  "from 2d or 3d  image with automatic dataNames",
  GridDataMakeFromArray@ImageData@i
];


DefinePublicFunction[
  GridDataMakeFromImage[dataNames_List, i : (_Image | _Image3D)]
  ,"custom dataNames, e.g. {r,g,b,a}"
  ,GridDataMakeFromArray[dataNames, ImageData@i]
];


DefinePublicFunction[
  GridDataMakeFromArrays[dataNames_List, (arrays : {__?ArrayQ})?(AllEqual[Dimensions])]
  ,"Make a dense grid of data from a non-interleaving array (outermost indices correspond to dataNames)
"
  ,GridDataMakeFromArray[dataNames, ArrayInterleave@arrays]
];

DefinePublicFunction[
  GridDataMakeFromArrays[dataNames_List, arrays : {__}, level_Integer] /;
    AllTrue[arrays, IsArrayAtLevel[#, level]&] && AllEqual[arrays, Dimensions[#][[;;level]] &]
  ,"note that 'level' will be GDArrayDepth"
  ,GridDataMakeFromArray[dataNames, ArrayInterleave[arrays, level+1], level]
];

DefinePublicFunction[
  GridDataMakeFromArrays[arrays : {__}, level_Integer]
  ,"from arrays with automatic dataNames"
  ,GridDataMakeFromArrays[FMMakeListDomainNames[Length@arrays], arrays, level]
];

(* -- Purpose -- *)
(*
Make a dense grid of data from sampling a function on some uniform grid
*)
(*TODO*)


DefinePublicFunction[
  GridDataMakeFromRules[
    dataNames_List,
    rules : _[
      _[_(*position*) , {__} (*values)*)]
          ...
    ]
  ] /; AllTrue[rules, Length@dataNames === Length@Last@# &]


  ,"Make a sparse grid of numeric data

list of

position -> data (as a list)

type rules"

  ,{f=FiniteMappingMakeFromRules@rules}~With~GridData[dataNames, f]
];


DefinePublicFunction[
GridDataMakeFromRules[
  rules : _[
    _[_(*position*) , {__} (*values)*)]
        ...
  ]
] , "with automatic data names",
  FMMakeListDomainNames@Length@Last@First@rules ~ GridDataMakeFromRules~ rules
];


(* -- Purpose -- *)
(*

*)
(* TODO optimize the case of NumericQ data and/or sparsity
TODO check that the FiniteMapping always returns a List of data of always the same length*)

DefinePublicFunction[
GridDataMakeFromFiniteMapping[dataNames_List, f_FiniteMapping],"internal form",
  GridData[dataNames, f]
  ];

(* -- Purpose -- *)
(*
determine the range of indices that are valid/min/max
*)
(*TODO*)
CoordinateBounds

(* -- Purpose -- *)
(*
*)
(* TODO works only for data of type {__?NumericQ} *)
(* TODO works only for positive coordinates, creates large holes/borders when coordinates are large positive numbers
  -> cut out only the GDCoodinateBounds, Lookup each value, use a given default*)
(* What should we do with missing data? :> default value*)

DefinePublicFunction[

  GDToArray[g : GridData[dataNames_List, f_FiniteMapping], extractedDataNames_List]
  /; GDDataNames@g~ContainsAll~extractedDataNames

  ,"Convert back to a (dense) array by subsampling at each valid point"

  , Module[
      {
        extractedPositions = Positions[dataNames, extractedDataNames]
        , mincb = First/@GDCoordinateBounds@g
        , toArrayPosition
      },

    toArrayPosition[p_] := (p - mincb) + 1;
    SparseArray@Flatten@Cases[f // FMAsRules,
      _[position_, values_] :>
          MapIndexed[toArrayPosition@position~Append~First@#2 -> #1 &,
            values[[extractedPositions]]
          ]
      ]
    ]

];

(* TODO only works with single number or vector valued attributes *)
(* TODO this will conflict with the above for List type variable names, which are standard. Use another name:
no need for usability shortcuts at this stage*)

DefinePublicFunction[
GDSingleDatumToArray[g : GridData[dataNames_List, f_FiniteMapping], extractedDataName_]
  /; GDDataNames@g~Contains~extractedDataName
  ,"for a single dataName"
  ,Module[{
    extractedPosition = First@First@Position[dataNames, extractedDataName]
    , mincb = First/@ GDCoordinateBounds@g
    , toArrayPosition
  },
    toArrayPosition[p_] := (p - mincb) + 1;

    SparseArray@Flatten@Cases[f // FMAsRules,
      _[position_, values_] :>
          If[ListQ@values[[extractedPosition]],
            MapIndexed[toArrayPosition@position~Append~First@#2 -> #1 &,
              values[[extractedPosition]]
            ]
            ,{toArrayPosition@position->values[[extractedPosition]]}
            ]
    ]
  ]
];

DefinePublicFunction[
GDToArray[g_GridData]
,"note that the coordinates will change, arrays only support 1-based positve indexing"
,GDToArray[g, GDDataNames@g]
];


(* TODO should this return a SparseArray or a dense on with the default already inserted?
Can Sparse Array support lists as data? *)


DefinePublicFunction[
  GDToImage[g_GridData] /; {1,2,3,4}~MemberQ~Length@GDDataNames@g && GDArrayDepth@g == 2
  ,"Visualize data as an image.
Assumes data at each point is a list of numbers, individual attributes are thus single numbers"
  ,Image[GDToArray[g], ColorSpace -> "RGB"] (* TODO unsparsify ? to array already does this*)
];

DefinePublicFunction[
  GDToImage3D[g_GridData] /;
    {1,2,3,4}~MemberQ~Length@GDDataNames@g && GDArrayDepth@g == 3
  ,"same for 3d image"
  ,Image3D[GDToArray[g], ColorSpace -> "RGB"]
];


DefinePublicFunction[
GDAsRules[GridData[dataNames_List, f_FiniteMapping]]
,"Representation in the format expected by GridDataMakeFromRules

Note that this loses dataNames"
,FMAsRules@f
];


DefinePublicFunction[
GDAsRulesOfRules[g : GridData[dataNames_List, f_FiniteMapping]]
  ,"rules of lists of rules,
 position -> dataName -> value"
  ,MapAt[
  Thread@Rule[dataNames, #] &,
  GDAsRules@g,
  {All, 2}
  ]
];

DefinePublicFunction[
  GDAsPairedRules[g : GridData[dataNames_List, f_FiniteMapping], pairing_ : List]
  ,"list of rules of the form
pairing[position, dataName] -> dataValue"

  ,Flatten[
    Table[pairing[#1, dataNames[[i]]] -> #2[[i]], {i, Length@dataNames}] & @@@ GDAsRules@g
  , 1]
];

DefinePublicFunction[
  GDAsPairedAtomRules[g : GridData[dataNames_List, f_FiniteMapping], pairing_ : List]
,"
list of rules of the form
pairing[position, dataName, atomPosition] -> dataValue-atomValue

i.e. each dataValue is further destructured into its (non-head) atoms"
,Flatten[
  Table[
    pairing[#1, dataNames[[i]], First@atomRule] -> Last@atomRule
    , {i, Length@dataNames}
    , {atomRule, PositionsToExpressionsOnLevel[#2[[i]], {-1}]}
  ] & @@@ GDAsRules@g
  , 2]
];

DefinePublicFunction[
GDAsArraysRule[g : GridData[dataNames_List, f_FiniteMapping]]

  ,"same as GDToArrays, but keeps dataNames"
  ,Thread@Rule[dataNames,GDToArrays@g]
];


(* TODO check compatibility *)
DefinePublicFunction[
GDUpdate[g1_GridData, g2_GridData]
    /; GDDataNames@g1 == GDDataNames@g2 && GDArrayDepth@g1 === GDArrayDepth@g2
  ,"Create a new gridData with data from g2 if present, defaulting to g1 data if missing there.
"
  ,    GridDataMakeFromRules[GDDataNames@g1, GDAsRules@g1 ~UpdateRuleList~ GDAsRules@g2]
];


DefinePublicFunction[
GDArrayDepth[g : GridData[dataNames_List, f_FiniteMapping]]
  ,"Length of position specifications

Dimensionality"
  ,Length@First@FMDomain@f
  ];

(*TODO use GDToArray specifically made to extract only one attribute*)
DefinePublicFunction[
GDToNoninterleavingArray[g_GridData]
  ,"Create a non-interleaving array, where dataNames are equivalent to the outermost instead of the innermost indices.
"
  ,GDSingleDatumToArray[g, #] & /@ GDDataNames@g
];


DefinePublicFunction[
GDDomain[GridData[dataNames_List, f_FiniteMapping]]
  ,"list all valid positions"
  ,FMDomain@f
];

DefinePublicFunction[
GDCoordinateBounds[g_GridData]
  ,"coordinate bounds of domain"
  ,CoordinateBounds@GDDomain@g
  ];

DefinePublicFunction[
GDCoordinateBoundingBox[g_GridData]
  ,"alternate coordinate bounds"
  ,CoordinateBoundingBox@GDDomain@g

  ];


DefinePublicFunction[
GDDataNames[GridData[dataNames_List, f_FiniteMapping]]
  ,"list all valid data names"
  ,dataNames
];

(*GDCoordinatePattern[g_GridData] := {__Integer}?(LengthQ[GDArrayDepth@g]);*)

(* -- Purpose -- *)
DefinePublicFunction[
GDCoordinateQ[g_GridData, c : {__Integer}] /; LengthQ[c, GDArrayDepth@g]
  ,"Determine whether an expression is possibly a valid coordinate of the grid g.

For this, it must be a list of Integers and have Length = Array depth."
  , True
];

DefinePublicFunction[
GDCoordinateQ[g_GridData, c_]
  ,"most things are not coordinates"
  ,False
];


DefinePublicFunction[
  GDLookup[
    g_GridData
    , position : {__Integer}
  ] /; g~GDCoordinateQ~position

  ,"get data at point as FiniteMapping"

  ,FiniteMappingMakeFromLists[GDDataNames@g, GDLookupList[g,position]]

];


DefinePublicFunction[
GDLookupList[
  g : GridData[dataNames_List, f_FiniteMapping]
  , position : {__Integer}
] /; g~GDCoordinateQ~position

,"get data at point as List in same order as DataNames

This gives the RHS expected in the rule constructors"

,f~FMEvaluate~position
];


DefinePublicFunction[
GDSubset[GridData[dataNames_List, f_FiniteMapping], positions : {{__Integer}..}]

,"Create a new grid from a subset of the positions."

,{g=FMDomainSubset[f, positions]}~With~
    GridDataMakeFromFiniteMapping[dataNames, g]
];


DefinePublicFunction[
GDSubsetCoordinateBounds[g_GridData, cb_List]
  ,"Select a range given by a bounding box"
  ,GDSubset[g,FlatCoordinateBoundsArray[cb]]
  ];


DefinePublicFunction[
GDDataNamesLength[GridData[dataNames_List, f_FiniteMapping]]
  ,"count all valid data names"
  ,Length@dataNames
  ];

DefinePublicFunction[
GDToFiniteMapping[GridData[dataNames_List, f_FiniteMapping]]
  ,"return a finite mapping that returns the data as a list at each point"
  ,f
  ];


DefinePublicFunction[
GDToNestedFiniteMapping[g : GridData[dataNames_List, f_FiniteMapping]]
,"return a finite mapping that returns a FiniteMapping: dataNames -> data at each point"
,FiniteMappingMakeFromLists[dataNames, #] ~FMMapValues~ GDToFiniteMapping@g
];

DefinePublicFunction[
GDMap[g_, GridData[dataNames_List, f_FiniteMapping], newDataNames_ : Null]

  ,"call f(data_FiniteMapping) at each point,
  data is passed as a FiniteMapping: dataNames -> data
and returned as a FiniteMapping: newDataNames -> newData"

  ,GridDataMakeFromFiniteMapping[
  FirstNonNull[newDataNames, dataNames],
  FMMapValues[FMEvaluateAll@g@FiniteMappingMakeFromLists[dataNames,#]&, f]
]
  ];


DefinePublicFunction[
GDMapList[g_, GridData[dataNames_List, f_FiniteMapping], newDataNames_ : Null]
,
"call g(data) at each point, data is passed as a list

if the amount of vectors changes, a new dataNames must be given"
,GridDataMakeFromFiniteMapping[
  FirstNonNull[newDataNames, dataNames],
  FMMapValues[g, f]
]
];

DefinePublicFunction[
GDDataSubset[g : GridData[dataNames_List, f_FiniteMapping], dataNamesSubset_List]
    /; ContainsAll[dataNames, dataNamesSubset]

,"create a new GridData containing only the dataNames in dataNamesSubset"
,  GDMap[
      FiniteMappingMakeFromLists[
        dataNamesSubset,
        #~FMEvaluateMultiple~dataNamesSubset
      ] & ,
      g, dataNamesSubset
      ]
];


DefinePublicFunction[
GDMapPositions[g_, GridData[dataNames_List, f_FiniteMapping]]

  ,
  "call g(position) at each position

This may modify the dimensionality of the corrdinates, but must keep them coordinates (lists of integers)"


  ,GridDataMakeFromFiniteMapping[dataNames,
  FMMapDomain[g, f]
  ]

];


DefinePublicFunction[
GDPrependDimension[g_GridData, x0_Integer : 1]
   , "  {position} -> {x0, position}

  x0 defaults to 1"
  ,GDMapPositions[#~Prepend~x0 &, g]
  ];

DefinePublicFunction[
GDRestDimension[g_GridData]
, " {x0, position} -> {position}

  Warning: Repeated keys might arise when there is more than one x0.
  Consider selecting first."
,GDMapPositions[Rest, g]
];



DefinePublicFunction[
GDAppendDimension[g_GridData, x0_Integer : 1]

  ,"{position} -> {position, x0}

    x0 defaults to 1"

  ,GDMapPositions[#~Append~x0 &, g]
];


DefinePublicFunction[
GDMostDimension[g_GridData]
  ,"{position, x0} -> {position}"
  ,GDMapPositions[Most, g]
];


(*TODO*)

DefinePublicFunction[
GDSelect[GridData[dataNames_List, f_FiniteMapping], select_]

  ,"Call select[FiniteMapping[dataNames, data], position] on each valid position.
Keep only those where select returns True.

This sparsifies the array."


  ,GridDataMakeFromFiniteMapping[
  dataNames,
  FMRuleCases[f, _[position_, dataList_] /; select[FiniteMappingMakeFromLists[dataNames,dataList], position]]
]
];

DefinePublicFunction[
GDSelectList[GridData[dataNames_List, f_FiniteMapping], select_]
  ,"same, but passes data as a list"

  ,GridDataMakeFromFiniteMapping[
  dataNames,
  FMRuleCases[f, _[position_, dataList_] /; select[dataList, position]]
]
];

(* -- Purpose -- *)
(*
Hollow out the array by keeping only those elements which don't have all neighbors defined.

This generalizes innerOutline Filtering.
*)
(*TODO*)
GDEdgeFilter

(* -- Purpose -- *)
(*
Drop all entries which don't have all neighbors defined

TODO allow specifying whether diagonal neighbors should be considered
*)
(*TODO*)
GDErode

(* -- Purpose -- *)
(*
Combine two grids of the same dimension into a new grid by applying a custom operation
to the data from both at each point.

TODO What do we do with points defined in one grid and not the other? Pass Missing[position]?
*)

DefinePublicFunction[
GDCopy[g_GridData,
  pastePositionOffset : {__Integer}] /;
    g~GDCoordinateQ~pastePositionOffset

  ,"Create a new grid that has all of the original data plus copies of all positions in src moved by pastePositionOffset

  The behaviour is undefined if the source and destination regions overlap."

  ,GDCopy[g, GDCoordinateBounds@g, pastePositionOffset]
];

DefinePublicFunction[
GDCopy[g_GridData,
  srcCoordinateBounds : {{_Integer, _Integer} ..},
  pastePositionOffset : {__Integer}] /;
    g~GDCoordinateQ~pastePositionOffset

  ,"same, but with custom bounds"

  ,GDCopy[g, FlatCoordinateBoundsArray@srcCoordinateBounds, pastePositionOffset]
];


DefinePublicFunction[
GDCopy[g_GridData,
  srcCoordinates : {{__Integer} ..},
  pastePositionOffset : {__Integer}] /;
    g~GDCoordinateQ~pastePositionOffset

  ,"same, but with a list of positions to copy"

  ,GridDataMakeFromRules[
      GDDataNames@g,
      GDAsRules[g]~Join~
          (#+pastePositionOffset -> GDLookupList[g, #] & /@ srcCoordinates)
  ]

];


(*TODO

TODO allow specifying a maximum distance instead/in addition,
or apply custom function at each new location*)

DefinePublicFunction[
GDNearestFilter[g_GridData]
  ,"full bounds"
  ,GDNearestFilter[g, GDCoordinateBounds@g]
 ];

DefinePublicFunction[
GDNearestFilter[g_GridData,
  newCoordinateBoundsArrayParam : {{_Integer, _Integer} ..}] /;
    Length@newCoordinateBoundsArrayParam == GDArrayDepth@g

  ,"custom bounds"

  , GDNearestFilter[g,
      FlatCoordinateBoundsArray@newCoordinateBoundsArrayParam (* TODO creating this explicitly is pretty inefficient, use an iterator *)
    ]
];


DefinePublicFunction[
GDNearestFilter[g : GridData[dataNames_List, f_FiniteMapping],
  newCoordinates : {{__Integer} ..}
] /; Length@First@newCoordinates == GDArrayDepth@g

  ,"Extend the existing data to fill the whole region specified by newBounds,
using for unknown values the nearest known value.

Generalizes NearestFilter which works only with Images."
  ,With[
      {
        nf = {n = Nearest@GDDomain@g}~With~(First@n@# &)
      },

      GridDataMakeFromRules[dataNames,
        Map[
          # ->
              g~GDLookupList~nf[#] &
          , newCoordinates
        ]
      ]

    ]
];


End[];

EndPackage[];