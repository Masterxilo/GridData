<<FiniteMapping`
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
- and or allow efficient dense indexing of small neighborhoods (for optimization objective functions)
- treat the case of NumericVector valued data (i.e. a list of floats) specially
 ^ consider implementing it as if this where the case even for differently nested lists (
 e.g. data of the form { {nx, nx}, d, {r,g,b}} or even with a matrix
*)

(* -- Purpose -- *)
(*
Store immutable data associated with positions on a uniform grid.
*)

(* Implementation notes *)
(* Valid forms are:
GridData[dataNames_List, f_FiniteMapping]
 f maps coordinates to points, on each point we store a list of values of length
 dataNames, which are associated with these names in their given order


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


(* Name *)
GridData

(* Attributes *)
GridData~SetAttributes~HoldAllComplete

(* -- Purpose -- *)
(*
Make a dense grid of data
*)

GridDataMakeFromArray[dataNames_List, array_, level_Integer]  /; IsArrayAtLevel[array, level]:= Module[{},
  Assert[Dimensions[array][[level+1]] == Length@dataNames];

  {f=FiniteMappingMakeFromArray[array, level]}~With~GridData[dataNames, f]

];

GridDataMakeFromArray[dataNames_List, array_?ArrayQ] := GridDataMakeFromArray[dataNames, array, ArrayDepth@array-1];


(* auto-naming *)
GridDataMakeFromArray[array_, level_Integer] /; IsArrayAtLevel[array, level] := Module[{dataNames},
  dataNames = FMMakeListDomainNames[ Dimensions[array][[level+1]] ];

  GridDataMakeFromArray[dataNames, array, level]

];

GridDataMakeFromArray[array_?ArrayQ] := GridDataMakeFromArray[array, ArrayDepth@array-1];

GridDataMakeFromImage[i : (_Image | _Image3D)] := GridDataMakeFromArray@ImageData@i;
GridDataMakeFromImage[dataNames_List, i : (_Image | _Image3D)] := GridDataMakeFromArray[dataNames, ImageData@i];

GridDataMakeFromImage3D = GridDataMakeFromImage


(* -- Purpose -- *)
(*
Make a dense grid of data from a non-interleaving array (outermost indices correspond to dataNames)
*)
GridDataMakeFromArrays[dataNames_List, (arrays : {__?ArrayQ})?(AllEqual[Dimensions])] :=
    GridDataMakeFromArray[dataNames, ArrayInterleave@arrays];

(* note that 'level' will be GDArrayDepth *)
GridDataMakeFromArrays[dataNames_List, arrays : {__}, level_Integer] /;
    AllTrue[arrays, IsArrayAtLevel[#, level]&] && AllEqual[arrays, Dimensions[#][[;;level]] &]:=
    GridDataMakeFromArray[dataNames, ArrayInterleave[arrays, level+1], level];

(* -- Purpose -- *)
(*
Make a dense grid of data from sampling a function on some uniform grid
*)
(*TODO*)


(* -- Purpose -- *)
(*
Make a sparse grid of numeric data

list of

position -> data (as a list)

type rules
*)

GridDataMakeFromRules[
  dataNames_List,
  rules : _[
    _[_(*position*) , {__} (*values)*)]
   ...
  ]
] := {f=FiniteMappingMakeFromRules@rules}~With~GridData[dataNames, f];


GridDataMakeFromRules[
  rules : _[
    _[_(*position*) , {__} (*values)*)]
        ...
  ]
] := rules ~ GridDataMakeFromRules~ FMMakeListDomainNames@Length@Last@First@rules


(* -- Purpose -- *)
(*

*)
(* TODO optimize the case of NumericQ data and/or sparsity
TODO check that the FiniteMapping always returns a List of data of always the same length*)
GridDataMakeFromFiniteMapping[dataNames_List, f_FiniteMapping] := GridData[dataNames, f];

(* -- Purpose -- *)
(*
determine the range of indices that are valid/min/max
*)
(*TODO*)
CoordinateBounds

(* -- Purpose -- *)
(*
Convert back to a (dense) array by subsampling at each valid point
*)
(* TODO works only for data of type {__?NumericQ} *)
GDToArray[GridData[dataNames_List, f_FiniteMapping], extractedDataNames_List] := Module[{
  extractedPositions = Positions[dataNames, extractedDataNames]
},

  SparseArray@Flatten@Cases[f // FMAsRules,
    _[position_, values_] :>
        MapIndexed[position~Append~First@#2 -> #1 &,
          values[[extractedPositions]]
        ]
    ]
];

(* TODO only works with single number or vector valued attributes *)
GDToArray[GridData[dataNames_List, f_FiniteMapping], extractedDataName_] := Module[{
  extractedPosition = First@First@Position[dataNames, extractedDataName]
},

  SparseArray@Flatten@Cases[f // FMAsRules,
    _[position_, values_] :>
        If[ListQ@values[[extractedPosition]],
          MapIndexed[position~Append~First@#2 -> #1 &,
            values[[extractedPosition]]
          ]
          ,{position->values[[extractedPosition]]}
          ]
  ]
];

(* note that the coordinates will change, arrays only support 1-based positve indexing *)
GDToArray[g_GridData] := GDToArray[g, GDDataNames@g]

GDToArrays;


(* -- Purpose -- *)
(*
Visualize data as an image.
Assumes data at each point is a list of numbers, individual attributes are thus single numbers
*)
GDToImage[g : GridData[dataNames_List, f_FiniteMapping]] /;
    {1,2,3,4}~MemberQ~Length@dataNames && GDArrayDepth@g == 2 :=
        Image[GDToArray[g], ColorSpace -> "RGB"] (* TODO unsparsify ? to array already does this*)


GDToImage3D[g_GridData] /;
    {1,2,3,4}~MemberQ~Length@dataNames && GDArrayDepth@g == 3 :=
        Image3D[GDToArray[g], ColorSpace -> "RGB"]

(* Note that this loses dataNames *)
GDAsRules[GridData[dataNames_List, f_FiniteMapping]] := FMAsRules@f

(* rules of lists of rules,
 position -> dataName -> value*)
GDAsRulesOfRules[g : GridData[dataNames_List, f_FiniteMapping]] := MapAt[
  Thread@Rule[dataNames, #] &,
  GDAsRules@g,
  {All, 2}
  ];

(*
list of rules of the form
pairing[position, dataName] -> dataValue
*)
GDAsPairedRules[g : GridData[dataNames_List, f_FiniteMapping], pairing_ : List] := Flatten[
    Table[pairing[#1, dataNames[[i]]] -> #2[[i]], {i, Length@dataNames}] & @@@ GDAsRules@g
  , 1]

(*
list of rules of the form
pairing[position, dataName, atomPosition] -> dataValue-atomValue

i.e. each dataValue is further destructured into its (non-head) atoms
*)
GDAsPairedAtomRules[g : GridData[dataNames_List, f_FiniteMapping], pairing_ : List] := Flatten[
  Table[
    pairing[#1, dataNames[[i]], First@atomRule] -> Last@atomRule
    , {i, Length@dataNames}
    , {atomRule, PositionsToExpressionsOnLevel[#2[[i]], {-1}]}
  ] & @@@ GDAsRules@g
  , 2]

(* same as GDToArrays, but keeps dataNames *)
GDAsArraysRule[g : GridData[dataNames_List, f_FiniteMapping]] := Thread@Rule[dataNames,GDToArrays@g]


(* -- Purpose -- *)
(*
Length of position specifications

Dimensionality
*)
GDArrayDepth[g : GridData[dataNames_List, f_FiniteMapping]] := Length@First@FMDomain@f

(* -- Purpose -- *)
(*
Create a non-interleaving array, where dataNames are equivalent to the outermost instead of the innermost indices.
*)
(*TODO*)
GDToNoninterleavingArray

(* -- Purpose -- *)
(*
list all valid positions
*)

GDDomain[GridData[dataNames_List, f_FiniteMapping]] := FMDomain@f;

GDCoordinateBounds[g_GridData] := CoordinateBounds@GDDomain@g
GDCoordinateBoundingBox[g_GridData] := CoordinateBoundingBox@GDDomain@g


(* -- Purpose -- *)
(*
list all valid data names
*)

GDDataNames[GridData[dataNames_List, f_FiniteMapping]] := dataNames;

(*GDCoordinatePattern[g_GridData] := {__Integer}?(LengthQ[GDArrayDepth@g]);*)

(* -- Purpose -- *)
(*
get data at point as FiniteMapping
*)
GDLookup[
  g : GridData[dataNames_List, f_FiniteMapping]
  , position : {__Integer}
] /; LengthQ[position, GDArrayDepth@g] := FiniteMappingMakeFromLists[dataNames, GDLookupList[g,position]]
(*
get data at point as List in same order as DataNames
*)
GDLookupList[
  g : GridData[dataNames_List, f_FiniteMapping]
  , position : {__Integer}
] /; LengthQ[position, GDArrayDepth@g]  := f~FMEvaluate~position


(* -- Purpose -- *)
(*
Create a new grid from a subset of the positions.
*)
GDSubset[GridData[dataNames_List, f_FiniteMapping], positions : {{__Integer}..}] := {g=FMDomainSubset[f, positions]}~With~
    GridDataMakeFromFiniteMapping[dataNames, g];

(* -- Purpose -- *)
(*
Select a range given by a bounding box
*)
GDSubsetCoordinateBounds[g_GridData, cb_List] := GDSubset[g,FlatCoordinateBoundsArray[cb]]

(* -- Purpose -- *)
(*
count all valid data names
*)

GDDataNamesLength[GridData[dataNames_List, f_FiniteMapping]] := Length@dataNames;

(* -- Purpose -- *)
(*
return a finite mapping that returns the data as a list at each point
*)
GDToFiniteMapping[GridData[dataNames_List, f_FiniteMapping]] := f;

(* -- Purpose -- *)
(*
return a finite mapping that returns a FiniteMapping: dataNames -> data at each point
*)
GDToNestedFiniteMapping[g : GridData[dataNames_List, f_FiniteMapping]] := FiniteMappingMakeFromLists[dataNames, #] ~FMMapValues~ GDToFiniteMapping@g


(* -- Purpose -- *)
(*
call f(data_FiniteMapping) at each point,
  data is passed as a FiniteMapping: dataNames -> data
and returned as a FiniteMapping: newDataNames -> newData
*)
GDMap[g_, GridData[dataNames_List, f_FiniteMapping], newDataNames_ : Null] :=  GridDataMakeFromFiniteMapping[
  FirstNonNull[newDataNames, dataNames],
  FMMapValues[FMEvaluateAll@g@FiniteMappingMakeFromLists[dataNames,#]&, f]
];

(* -- Purpose -- *)
(*
call f(data) at each point, data is passed as a list

if the amount of vectors changes, a new dataNames must be given
*)
GDMapList[g_, GridData[dataNames_List, f_FiniteMapping], newDataNames_ : Null] :=  GridDataMakeFromFiniteMapping[
  FirstNonNull[newDataNames, dataNames],
  FMMapValues[g, f]
];

(*
create a new GridData containing only the dataNames in dataNamesSubset
*)
GDDataSubset[g : GridData[dataNames_List, f_FiniteMapping], dataNamesSubset_List] /; ContainsAll[dataNames, dataNamesSubset] :=
    GDMap[
      FiniteMappingMakeFromLists[
        dataNamesSubset,
        #~FMEvaluateMultiple~dataNamesSubset
      ] & ,
      g, dataNamesSubset
      ];

(* -- Purpose -- *)
(*
call g(position) at each position

This may modify the dimensionality of the corrdinates, but must keep them coordinates (lists of integers)
*)
GDMapPositions[g_, GridData[dataNames_List, f_FiniteMapping]] := GridDataMakeFromFiniteMapping[dataNames,
  FMMapDomain[g, f]
  ];

(*
  {position} -> {x0, position}
*)
GDPrependDimension[g_GridData, x0_Integer : 1] := GDMapPositions[#~Prepend~x0 &, g];

(*
  {position} -> {position, x0}
*)
GDAppendDimension[g_GridData, x0_Integer : 1] := GDMapPositions[#~Append~x0 &, g];

(* -- Purpose -- *)
(*
Call select[FiniteMapping[dataNames, data], position] on each valid position.
Keep only those where select returns True.

This sparsifies the array.
*)
(*TODO*)
GDSelect[GridData[dataNames_List, f_FiniteMapping], select_] :=GridDataMakeFromFiniteMapping[
  dataNames,
  FMRuleCases[f, _[position_, dataList_] /; select[FiniteMappingMakeFromLists[dataNames,dataList], position]]
];

GDSelectList[GridData[dataNames_List, f_FiniteMapping], select_] := GridDataMakeFromFiniteMapping[
  dataNames,
  FMRuleCases[f, _[position_, dataList_] /; select[dataList, position]]
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

(* -- Purpose -- *)
(*
Extend the existing data to fill the whole region specified by newBounds,
using for unknown values the nearest known value.

Generalizes NearestFilter which works only with Images.
*)
(*TODO

TODO allow specifying a maximum distance instead/in addition,
or apply custom function at each new location*)

GDNearestFilter[g_GridData] := GDNearestFilter[g, GDCoordinateBounds@g]

GDNearestFilter[g_GridData,
  newCoordinateBoundsArrayParam : {{_Integer, _Integer} ..}] /;
    Length@newCoordinateBoundsArrayParam == GDArrayDepth@g :=
    GDNearestFilter[g,
      CoordinateBoundsArray@newCoordinateBoundsArrayParam ~Level~{-2} (* TODO this is pretty inefficient, use an iterator *)
    ];



GDNearestFilter[g : GridData[dataNames_List, f_FiniteMapping],
  newCoordinates : {{__Integer} ..}
] /; Length@First@newCoordinates == GDArrayDepth@g :=
    With[
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