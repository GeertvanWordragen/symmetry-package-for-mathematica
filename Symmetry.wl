(* ::Package:: *)

(* ::Title:: *)
(*Symmetry*)


(* ::Author:: *)
(*Geert van Wordragen*)


(* ::Text:: *)
(*\[Copyright] Copyright 2019 Eindhoven University of Technology, The Netherlands*)
(*This software is made available under the terms of the MIT License.*)


BeginPackage["Symmetry`"];


(* ::Subsection::Closed:: *)
(*Usage Messages*)


(* ::Subsubsection::Closed:: *)
(*Symmetry*)


SymmetryPermutation::usage =
	"SymmetryPermutation[Polygon] gives a PermutationGroup \
	describing the symmetry of the 2D polygon.
	SymmetryPermutation[Point] gives a PermutationGroup \
	describing the symmetry of the given (2D or 3D) point set.
	SymmetryPermutation[List] gives a PermutationGroup \
	describing the symmetry of the given list of (2D or 3D) points.
	SymmetryPermutation[MeshRegion] gives a PermutationGroup \
	describing the symmetry of the given (2D or 3D) mesh.
	SymmetryPermutation[BoundaryMeshRegion] gives a PermutationGroup \
	describing the symmetry of the given (2D or 3D) boundary mesh.";

SymmetryTransformation::usage =
	"SymmetryTransformation[Polygon] gives a list of associations \
	describing the symmetry of the 2D polygon.
	SymmetryTransformation[Point] gives a list of associations \
	describing the symmetry of the given (2D or 3D) point set.
	SymmetryTransformation[List] gives a list of associations \
	describing the symmetry of the given list of (2D or 3D) points.
	SymmetryTransformation[MeshRegion] gives a list of associations \
	describing the symmetry of the given (2D or 3D) mesh.
	SymmetryTransformation[BoundaryMeshRegion] gives a list of associations \
	describing the symmetry of the given (2D or 3D) boundary mesh.";


(* ::Subsubsection::Closed:: *)
(*TransformationGroup*)


Rotation[];
Reflection[];
TransformationGroup[];

Rotation3D[];
Reflection3D[];
TransformationGroup3D[];


ToTransformations::usage = "";


TransformationGroupGenerators::usage = "";
TransformationGroupElements::usage = "";

RotationAngle::usage = "";
RotationPoint::usage = "";
RotationAxis::usage = "";

ReflectionPoint::usage = "";
ReflectionNormal::usage = "";
ReflectionLine::usage = "";
ReflectionPlane::usage = "";

TransformationPrimitive::usage = "";

PolyhedronSymmetry::usage = "";
MeshGraph::usage="";


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Options*)


With[{
	options = {
		Tolerance -> 10^-6,
		WorkingPrecision -> Automatic,
		AdditionalData -> None,
		DataReflection -> Identity,
		SameTest -> Equal
	}},
	Options[SymmetryPermutation] = options;
	Options[SymmetryTransformation] = options;
	Options[PolygonSymmetry] = options;
	Options[PointSetSymmetry] = options;
	Options[PointSetSymmetry2D] = options;
	Options[PointSetSymmetry3D] = options;
	Options[PointSetSymmetryAxis] = options;
	Options[BoundaryMeshSymmetry] = options;
	Options[MeshSymmetry] = options;
	Options[MeshSymmetry2D] = options;
	Options[PolyhedronSymmetry] = options;
]


(* ::Subsection::Closed:: *)
(*Function definitions*)


SymmetryPermutation[P_Polygon, ops:OptionsPattern[]] :=
	PolygonSymmetry[PolygonCoordinatesOrdered[P], True, ops]
SymmetryTransformation[P_Polygon, ops:OptionsPattern[]] :=
	PolygonSymmetry[PolygonCoordinatesOrdered[P], False, ops]

SymmetryPermutation[P_Point, ops:OptionsPattern[]] :=
	PointSetSymmetry[First@P, True, ops]
SymmetryTransformation[P_Point, ops:OptionsPattern[]] :=
	PointSetSymmetry[First@P, False, ops]
SymmetryPermutation[P_List, ops:OptionsPattern[]] :=
	PointSetSymmetry[P, True, ops]
SymmetryTransformation[P_List, ops:OptionsPattern[]] :=
	PointSetSymmetry[P, False, ops]

SymmetryPermutation[m_BoundaryMeshRegion, ops:OptionsPattern[]] :=
	BoundaryMeshSymmetry[m, True, ops]
SymmetryTransformation[m_BoundaryMeshRegion, ops:OptionsPattern[]] :=
	BoundaryMeshSymmetry[m, False, ops]
SymmetryPermutation[m_MeshRegion, ops:OptionsPattern[]] :=
	MeshSymmetry[m, True, ops]
SymmetryTransformation[m_MeshRegion, ops:OptionsPattern[]] :=
	MeshSymmetry[m, False, ops]


(*
	pre: P is a list of either only 2D coordinates or only 3D coordinates
	ret: a PermutationGroup or list of associations representing the symmetry of P
*)
PointSetSymmetry[P_List, asGroup_?BooleanQ, ops:OptionsPattern[]] :=
	If[Length[P[[1]]] == 2,
		PointSetSymmetry2D[P, asGroup, ops],
		PointSetSymmetry3D[P, asGroup, ops]
	]

(*
	pre: m is a valid 2D or 3D mesh region
	ret: a PermutationGroup or list of associations representing the symmetry of m
*)
MeshSymmetry[m_MeshRegion, asGroup_?BooleanQ, ops:OptionsPattern[]] :=
 	If[RegionEmbeddingDimension[m] == 2,
		MeshSymmetry2D[m, asGroup, ops],
		PolyhedronSymmetry[m, asGroup, ops]
	]

(*
	pre: m is a valid 3D boundary mesh region
		or a 2D boundary mesh region equivalent to a polygon
	ret: a PermutationGroup or list of associations representing the symmetry of m
*)
BoundaryMeshSymmetry[m_BoundaryMeshRegion, asGroup_?BooleanQ, ops:OptionsPattern[]] :=
	If[RegionEmbeddingDimension[m] == 2,
		PolygonSymmetry[First[MeshPrimitives[m, 2]], asGroup, ops],  (* TODO: PolygonSymmetry for multiple polygons? *)
		PolyhedronSymmetry[m, asGroup, ops]
	]


(* ::Subsection::Closed:: *)
(*Helper functions*)


(*
	pre: p is a single polygon without holes
	ret: the polygon's vertex coordinates in order
*)
PolygonCoordinatesOrdered[p_Polygon] :=
	If[Length[p] == 1, p[[1]], p[[1]][[p[[2]]]]]

(*
	pre: list and sublist contain elements whose equality can be tested with SameTest
	ret: the indices of the first n occurences of sublist in list
*)
SequencePositionTest[list_List, sublist_List, SameTest_, n_Integer:Infinity] :=
	If[SameTest === Equal,
		SequencePosition[list, sublist, n],
	
		With[{len = Length[sublist]},
			SequencePosition[list, x_List /; ListEqual[sublist, x, len, SameTest], n]
		]
	]

(*
	pre: Length[a] \[Equal] Length[b] \[Equal] length,
		a and b contain elements whose equality can be tested with SameTest
	ret: whether the given lists are equal under the given test
*)
ListEqual[a_List, b_List, length_Integer, SameTest_] :=
	Length[b] == length \[And] 
	AllTrue[Range[length], SameTest[a[[#]], b[[#]]]&]

(*
	pre: S and S2 are two equally long lists with elements that can be equal
	ret: the offset (> 0) that makes S the same as S2 or -1 if this never happens
*)
GetCyclicOffset[S_List, S2_List, SameTest_] := With[{
		out = SequencePositionTest[S[[2;;]] ~Join~ S, S2, SameTest, 1]
	},
	If[out == {}, 
		-1,
		out[[1, 1]]
	]
]

(*
	pre: S and S2 are two equally long lists with elements that can be equal
	ret: all offsets (> 0) that makes S the same as S2
*)
GetAllCyclicOffsets[S_List, S2_List, SameTest_] :=
	SequencePositionTest[S[[2;;]] ~Join~ S, S2, SameTest]

(*
	pre: x has a dot product
	ret: the squared norm of x
*)
NormSqr[x_] := x . x

(*
	pre: x is a number or a list of numbers
	ret: x in a comparable form with the given precision
*)
Prec[x_, precision_] := If[precision == Infinity, RootReduce[x], N[x, precision]]

(*
	pre: p contains the values given by the user
	ret: the working precision to use
*)
ExtractWorkingPrecision[workingPrecision_, p_List] :=
	If[workingPrecision === Automatic,
		Precision[p],
		workingPrecision
	]

(*
	pre: tolerance is a number or a pair of numbers
	ret: a pair containing the tolerances to use for distances and for angles
*)
ExtractTolerance[tolerance_, workingPrecision_] :=
		If[workingPrecision == Infinity,
			{0, 0},
			If[ListQ[tolerance],
				tolerance,
				{tolerance, tolerance}
			]
		];


(* ::Subsection::Closed:: *)
(*2D*)


(* ::Subsubsection::Closed:: *)
(*Polygon*)


(*
	pre: P is a list of two-dimensional vectors
		where each point is connected to its neighbours in the list
	ret: a representation of the symmetry of the polygon
*)
PolygonSymmetry[P_List, asGroup_?BooleanQ, ops:OptionsPattern[]] := With[{
		n = Length[P],
		centroid = Mean[P]
	},
	Block[{workingPrecision, distTolerance, angleTolerance, Pc, S, k, j},
		workingPrecision = ExtractWorkingPrecision[OptionValue[WorkingPrecision], P];
		{distTolerance, angleTolerance} = ExtractTolerance[
			OptionValue[Tolerance], workingPrecision
		];
		sameTest = PolygonSameTest[
			distTolerance, angleTolerance,
			OptionValue[SameTest]
		];
		
		Pc = Map[# - centroid &, P];
		S = PolygonEncode[Pc, OptionValue[AdditionalData], workingPrecision];
		k = GetCyclicOffset[S, S, sameTest];
		j = PolygonReflection[S, k, PolygonReverse, sameTest];
		(* NB: as opposed to the paper, j is an offset instead of an index. *)
	
		If[asGroup,
			PermutationGroup@Append[
				Map[GetReflectionCycle[n], j],
				GetRotationCycle[k, n]
			],
			TransformationGroup@Append[
				Map[GetReflectionTransform[centroid, Pc, n], j],
				GetRotationTransform[centroid, k, n]
			]
		]
	]
]


(*
	pre: P a centralised list of two-dimensional vector representing a polygon,
		D either Nothing
		or a list of equal length containing additional data to assign to each point
	ret: a list of equal length, containing pairs or three-tuples
		of line lengths, angles, and possible additional data
*)
PolygonEncode[P_List, D_, precision_] := With[{n = Length[P]},
	Table[PolygonEncodeElement[P, D, precision, i, n], {i, n}]
]

PolygonEncodeElement[P_List, D_, precision_, i_Integer, n_Integer] := {
	Prec[Norm[P[[i]] - P[[Mod[i + 1, n, 1]]]], precision],
	Prec[PlanarAngle[P[[{Mod[i - 1, n, 1], i, Mod[i + 1, n, 1]}]]], precision],
	If[D === None, Nothing, D[[i]]]
}

(*
	pre: S a list of 2- or 3-tuples of line lengths, angles
		and possible additional information describing a polygon, length <= Length[S]
	ret: a list of the given length
		traversing the polygon described by S in opposite direction
*)
PolygonReverse[S_List, length_Integer] := With[{n = Length[S]},
	Table[
		{
			S[[n - i + 1, 1]],
			S[[Mod[n - i + 2, n, 1], 2]],
			If[Length[First@S] == 3,
				S[[Mod[n - i + 2, n, 1], 3]],
				Nothing
			]
		},
		{i, length}
	]
]

(*
	pre: S a list of tuples describing a polygon,
		k < Length[s] the shift after which S repeats itself,
		Rev a function that lets S's polygon be traversed in opposite direction
	ret: all index shifts that make S equal to its reversed variant
*)
PolygonReflection[S_List, k_Integer, Rev_, SameTest_] :=
	Map[First, GetAllCyclicOffsets[S[[1 ;; k]], Rev[S, k], SameTest]]


(* 
	pre: distTolerance and angleTolerance are small numbers,
		SameTest can compare the last part of both lists
	ret: test for the equality of {distance, angle, ...} tuples
*)
PolygonSameTest[0, 0, Equal] := Equal
PolygonSameTest[distTolerance_, angleTolerance_, SameTest_][a_List, b_List] :=
	Abs[a[[1]] - b[[1]]] <= distTolerance \[And]
	Abs[a[[2]] - b[[2]]] <= angleTolerance \[And]
	SameTest[a[[3;;]], b[[3;;]]]


(*
	pre: 
	ret: cycles describing the reflection 
*)
GetReflectionCycle[n_Integer][j_Integer] :=
	Cycles@Table[
		{i, Mod[2 + j - i, n, 1]},
		{i, Floor[j / 2] + 2, Ceiling[(n + j) / 2]}
	]

GetReflectionTransform[centroid_List, S_List, n_Integer][j_Integer] := With[{
		line = GetReflectionElement[S, j, n]
	},
	If[line != {0, 0},
		Reflection[InfiniteLine[centroid, line]],
		If[j >= n, Nothing,
			GetReflectionTransform[centroid, S, n][j + n]
		]
	]
]

GetReflectionElement[S_List, index_Integer, n_Integer] :=
	If[EvenQ[index],
		S[[1 + index / 2]],
		(S[[1 + (index - 1) / 2]] + S[[Mod[1 + (index + 1) / 2, n, 1]]]) / 2
	]

(*
	pre: k divides n
	ret: cycles describing n elements being shifted k places
*)
GetRotationCycle[k_Integer, n_Integer] :=
	If[k != n,
		Cycles@Table[Range[i, n, k], {i, k}],
		Nothing
	]

(*
	pre: k divides n
	ret: an association describing the rotational symmetry around centroid
		in n/k steps
*)
GetRotationTransform[centroid_List, k_Integer, n_Integer] :=
	If[k == n, Nothing,
		Rotation[centroid, 2\[Pi] k / n]
	]


(* ::Subsubsection::Closed:: *)
(*Point Set*)


(*
	pre: P is a list of 2D coordinates
	ret: a PermutationGroup or list of associations representing the symmetry of P
*)
PointSetSymmetry2D[P_List, asGroup_?BooleanQ, ops:OptionsPattern[]] := With[{
		n = Length[P],
		centroid = Mean[P]
	},
	Block[{workingPrecision, distTolerance, angleTolerance, Pc, \[CapitalGamma], S,
			ordering, o, k, axes},
		workingPrecision = ExtractWorkingPrecision[OptionValue[WorkingPrecision], P];
		{distTolerance, angleTolerance} = ExtractTolerance[
			OptionValue[Tolerance], workingPrecision
		];
		Same = If[OptionValue[DataReflection] === Identity,
			PointSetSameTest[angleTolerance],
			PointSetSameTest[angleTolerance, OptionValue[SameTest]]
		];
		
		Pc = Map[# - centroid &, P];
		{\[CapitalGamma], S, ordering} = PointSetEncode[
			Pc, OptionValue[AdditionalData],
			workingPrecision, distTolerance,
			OptionValue[SameTest],
			OptionValue[DataReflection] =!= Identity
		];
		
		o = Map[
			Length[#] / GCD[Length[#], GetCyclicOffset[#, #, Same]]&,
			S
		];
		k = Apply[GCD, o];

		axes = MapThread[
			PointSetReflection[k, workingPrecision, Same, OptionValue[DataReflection]],
			{\[CapitalGamma], S}
		];
		
		If[asGroup,
			With[{total = Length[axes]},
				axes = Flatten[axes, 1];
				axes = Gather[axes, Abs@Mod[First[#1] - First[#2], \[Pi]/k, -\[Pi]/(2k)] <= angleTolerance&];
				axes = Select[axes, Length[#] == total&];
				axes = Map[#[[2]]&, axes, {2}];
			];
			PermutationGroup@Append[
				Map[PointSetReflectionCycle[ordering], axes],
				PointSetRotationCycle[k, ordering]
			],
			
			axes = Map[First, axes, {2}];
			axes = Apply[
				Intersection[##, SameTest -> (Abs@Mod[#1 - #2, \[Pi]/k, -\[Pi]/(2k)] <= angleTolerance &)]&,
				axes
			];
			TransformationGroup@Append[
				Map[PointSetReflectionTransform[centroid], axes],
				PointSetRotationTransform[centroid, k]
			]
		]
	]
]


PointSetEncode[P_List, D_, precision_, distTolerance_, SameTest_, reflData_] := Block[{
		PD, \[CapitalGamma], S, ordering, SameDist
	},
	PD = MapIndexed[Append[#1, First[#2]]&, P];
	SameDist[x_List, y_List] :=
		Prec[Abs[Norm[x[[1;;2]]] - Norm[y[[1;;2]]]], precision] <= distTolerance;
		
	If[D === None \[Or] reflData,
		\[CapitalGamma] = Gather[PD, SameDist],
		
		PD = MapThread[Append[#1, #2]&, {PD, D}];
		\[CapitalGamma] = Gather[PD, SameDist[#1, #2] \[And] SameTest[#1[[4]], #2[[4]]]&]
	];
	\[CapitalGamma] = DeleteCases[\[CapitalGamma], x_/; NormSqr[x[[1, 1;;2]]] <= distTolerance^2, {1}];
	\[CapitalGamma] = Map[{ArcTan[#[[1]], #[[2]]], #[[3]]}&, \[CapitalGamma], {2}];
	\[CapitalGamma] = SortBy[#, First, NumericalOrder] &/@ \[CapitalGamma];
		
	ordering = Map[#[[2]]&, \[CapitalGamma], {2}];
	\[CapitalGamma] = Map[First, \[CapitalGamma], {2}];
	
	S = Map[PointSetEncodeCycle[precision], \[CapitalGamma]];
	If[reflData,
		S = MapThread[MapThread[Combine[D], {#1, #2}]&, {S, ordering}]
	];
	{\[CapitalGamma], S, ordering}
]

Combine[data_List][x_, index_] := {x, data[[index]]}

PointSetEncodeCycle[precision_][cycle_List] := With[{n = Length[cycle]},
	Table[
		Prec[Mod[cycle[[Mod[j + 1, n, 1]]] - cycle[[j]], 2\[Pi]], precision],
		{j, n}
	]
]


(* 
	pre: angleTolerance is a small number
	ret: test for the equality of angles
*)
PointSetSameTest[0] := Equal
PointSetSameTest[0, Equal] := Equal
PointSetSameTest[angleTolerance_][a_, b_] :=
	Abs[a - b] <= angleTolerance
PointSetSameTest[angleTolerance_, SameTest_][a_, b_] :=
	Abs[a[[1]] - b[[1]]] <= angleTolerance \[And] SameTest[a[[2]], b[[2]]]


PointSetReverse[Identity][P_List, length_Integer] := P[[length ;; 1 ;; -1]]
PointSetReverse[DataReflection_][P_List, length_Integer] := With[{n = Length[P]},
	Table[
		{
			P[[n - i + 1, 1]],
			DataReflection[P[[Mod[n - i + 2, n, 1], 2]]]
		},
		{i, length}
	]
]

PointSetReflection[k_Integer, precision_, SameTest_, DataReflection_][\[CapitalGamma]_List, S_List] := With[{
		j = PolygonReflection[S, Length[S] / k, PointSetReverse[DataReflection], SameTest]
	},
	Map[GetReflAngle[\[CapitalGamma], precision], j]
]

GetReflAngle[\[CapitalGamma]_List, precision_][index_Integer] := With[{
		\[Alpha] = GetReflectionElement[\[CapitalGamma], index, Length[\[CapitalGamma]]]
	},
	{Prec[\[Alpha], precision], index}
]

AngleAtIndex[\[CapitalGamma]_List, index_Integer, n_Integer] := With[{
		value = \[CapitalGamma][[Mod[index, n, 1]]]
	},
	If[ListQ[value],
		value[[1]],
		value
	]
]


MapCycle[ord_, cyc_] := Map[ord[[#]]&, cyc, {3}];

PointSetReflectionCycle[ordering_List][j_List] := With[{n = Map[Length, ordering]},
	Block[{cycle},
		cycle = MapThread[GetReflectionCycle[#1][#2]&, {n, j}];
		cycle = MapThread[MapCycle, {ordering, cycle}];
		PermutationProduct@@cycle
	]
]

PointSetReflectionTransform[centroid_List][axis_] :=
	Reflection[InfiniteLine[centroid, FromPolarCoordinates[{1, axis}]]]

PointSetRotationCycle[k_Integer, ordering_List] := If[k == 1, Nothing,
	With[{n = Map[Length, ordering]},
		Block[{cycle},
			cycle = Map[GetRotationCycle[# / k, #]&, n];
			cycle = MapThread[MapCycle, {ordering, cycle}];
			PermutationProduct@@cycle
		]
	]
]

PointSetRotationTransform[centroid_List, k_Integer] :=
	If[k == 1, Nothing,
		Rotation[centroid, 2\[Pi] / k]
	]


(* ::Subsubsection::Closed:: *)
(*Mesh*)


MeshSymmetry2D[m_MeshRegion, asGroup_?BooleanQ, ops:OptionsPattern[]] := Block[{
		v, g, data, Same, workingPrecision, distTolerance, ReflFunc},
	v = MeshCoordinates[m];
	g = MeshGraph[m];
	
	workingPrecision = ExtractWorkingPrecision[OptionValue[WorkingPrecision], v];
	distTolerance = First@ExtractTolerance[
		OptionValue[Tolerance], workingPrecision
	];
	
	data = Map[EncodeMeshConnections[v, g, Mean[v], distTolerance], Range[Length[v]]];
	
	If[OptionValue[AdditionalData] =!= None,
		data = MapThread[{#1, #2}&, {data, OptionValue[AdditionalData]}];
		Same = MeshEqual[distTolerance][#1[[1]], #2[[1]]] \[And] OptionValue[SameTest][#1[[2]], #2[[2]]]&;
		ReflFunc = {MeshReflFunc[distTolerance][#[[1]]], #[[2]]}&,
		
		Same = MeshEqual[distTolerance];
		ReflFunc = MeshReflFunc[distTolerance];
	];
	PointSetSymmetry[v, asGroup, AdditionalData -> data, SameTest -> Same,
		DataReflection -> ReflFunc, ops]
]

EncodeMeshConnections[v_List, g_Graph, centroid_List, distTolerance_][index_Integer] := Block[{
		adjacent, base
	},
	base = centroid - v[[index]];
	base = {base, Cross[base]};
	adjacent = AdjacencyList[g, index];
	adjacent = Map[
		base . (v[[#]] - v[[index]])&,
		adjacent
	];
	AdjSort[distTolerance][adjacent]
]

MeshReflFunc[distTolerance_][x_List] := AdjSort[distTolerance][Map[{#[[1]], -#[[2]]}&, x]]
AdjSort[distTolerance_][x_List] := Sort[x,
	If[Abs[#1[[1]] - #2[[1]]] > distTolerance,
		#1[[1]] < #2[[1]], #1[[2]] < #2[[2]]]&]
AdjSort[0][x_List] := Sort[x, NumericalOrder]

MeshEqual[distTolerance_][a_, b_] :=
	ListEqual[
		a, b, Length[a],
		Abs[#1[[1]] - #2[[1]]] <= distTolerance \[And]
		Abs[#1[[2]] - #2[[2]]] <= distTolerance&
	]


(* ::Subsection::Closed:: *)
(*3D*)


(* ::Subsubsection::Closed:: *)
(*Symmetry Axes*)


(*
	pre: m is 3D and has coordinates v
	ret: a list of possible symmetry axes, described by two orthogonal vectors
*)
SymmetryAxes[(m_BoundaryMeshRegion | m_MeshRegion), v_List, tolerance_:0] :=
	SymmetryAxes[v, MeshGraph[m], tolerance]

(*
	pre: v is 3D and there is a 1-to-1 correspondence
		between points in v and edges of g
	ret: a list of possible symmetry axes, described by two orthogonal vectors
*)
SymmetryAxes[v_List, g_Graph, tolerance_:0] :=
	SymmetryAxes[v, GraphAutomorphismGroup[g], tolerance]

SymmetryAxes[v_List, G_PermutationGroup, tolerance_:0] := Block[{axes},
	axes = Map[AxesFromPermutation[v, tolerance], GroupElements[G]];
	axes = Catenate[axes];
	GetNormals[x_List, y_List] :=
		First@Sort[{x[[1]] \[Cross] y[[2]], y[[2]] \[Cross] x[[1]]}];
	
	axes = If[tolerance == 0,
		DeleteDuplicatesBy[axes, GetNormals],
		DeleteDuplicatesBy[axes, Round[GetNormals[#[[1]], #[[2]]], tolerance]&]
	];
	Map[Orthogonalize, axes]
]

AxesFromPermutation[v_List, tolerance_][c_Cycles] := Block[{axes, normals},
	normals = {};
	axes = Map[AxesFromCycle[v], First[c]];
	If[Length[axes] == 0,
		normals = DeleteDuplicates[normals, 1 - Abs[#1 . #2] <= tolerance&];
		If[Length[normals] >= 2, {normals[[{1, 2}]]}, Nothing],
		
		Apply[Intersection[##, SameTest -> SameBasis[tolerance]]&, axes]
	]
]

AxesFromCycle[v_List][subcycle_List] :=
	If[Length[subcycle] < 3,
		If[Length[subcycle] == 2,
			AppendTo[normals, Normalize[v[[subcycle[[1]]]] - v[[subcycle[[2]]]]]]
		];
		Nothing,
			
		Map[
			{Normalize[#[[2]] - #[[1]]], Normalize[#[[3]] - #[[1]]]}&,
			PossiblePoints[v[[subcycle]]]
		]
	]

PossiblePoints[subcycle_List] :=
	Map[
		If[3# > Length[subcycle], Nothing,
			{subcycle[[#]], subcycle[[2#]], subcycle[[3#]]}
		]&,
		Divisors[Length[subcycle]]
	]

SameBasis[tolerance_][x_List, y_List] :=
	NormSqr[x[[1]] \[Cross] x[[2]] - y[[1]] \[Cross] y[[2]]] <= tolerance^2 \[Or]
	NormSqr[x[[1]] \[Cross] x[[2]] - y[[2]] \[Cross] y[[1]]] <= tolerance^2

MeshGraph[(m_BoundaryMeshRegion | m_MeshRegion)] := Block[{A},
	A = m["ConnectivityMatrix"[1, 0]];
	A = Transpose[A] . A;
	Do[A[[i, i]] = 0, {i, Length[A]}];
	A = Map[Unitize, A];
	AdjacencyGraph[A]
]

PolyhedronGraph[p_Polyhedron] := With[{
		graphs = Map[PathGraph[Append[#, First@#]]&, p[[2]]]
	},
	Apply[GraphUnion, graphs]
]


(* ::Subsubsection::Closed:: *)
(*Point Set*)


PointSetSymmetry3D[v_List, asGroup_?BooleanQ, ops:OptionsPattern[]] :=
	Block[{workingPrecision, distTolerance, axes, out},
		workingPrecision = ExtractWorkingPrecision[OptionValue[WorkingPrecision], v];
		distTolerance = First@ExtractTolerance[
			OptionValue[Tolerance], workingPrecision
		];
		axes = SymmetryAxes[
			v,
			PermutationGroup@GroupGenerators@SymmetricGroup[Length[v]],
			distTolerance
		];
		out = Map[PointSetSymmetryAxis[v, asGroup, ops], axes];
		If[asGroup,
			out = Catenate[Map[GroupGenerators, out]];
			PermutationGroup[out],
			
			out = Catenate[Map[TransformationGroupGenerators, out]];
			TransformationGroup3D[out]
		]
	]

PointSetSymmetryAxis[P_List, asGroup_?BooleanQ, ops:OptionsPattern[]][basis_List] :=
With[{axis = basis[[1]] \[Cross] basis[[2]]},
	Block[{workingPrecision, distTolerance, symm},
		workingPrecision = ExtractWorkingPrecision[OptionValue[WorkingPrecision], P];
		distTolerance = First@ExtractTolerance[
			OptionValue[Tolerance], workingPrecision
		];
		symm = PointSetSymmetry2D[
			Map[basis . #&, P],
			asGroup,
			AdditionalData -> If[OptionValue[AdditionalData] === None,
				Map[Prec[# . axis, workingPrecision]&, P],
				MapThread[
					{Prec[#1 . axis, workingPrecision], #2}&,
					{P, OptionValue[AdditionalData]}
				]
			],
			SameTest -> If[OptionValue[AdditionalData] === None,
				If[distTolerance == 0,
					Equal,
					Abs[#1 - #2] <= distTolerance&
				],
				If[distTolerance == 0 \[And] OptionValue[SameTest] === Equal,
					Equal,
					Abs[#1[[1]] - #2[[1]]] <= distTolerance \[And]
					OptionValue[SameTest][#1[[2]], #2[[2]]]&
				]
			],
			ops
		];
		If[asGroup, symm, Add3DData[basis, axis, symm]]
	]
]


Add3DData[basis_List, axis_List, symm_TransformationGroup] :=
	TransformationGroup3D@Map[
		Add3DData[basis, axis],
		TransformationGroupGenerators[symm]
	]

Add3DData[basis_List, axis_List][data_] :=
	If[Head[data] === Rotation,
		With[{point = First[RotationPoint[data]]},
			Rotation3D[InfiniteLine[point . basis, axis], RotationAngle[data]]
		],
		With[{line = ReflectionLine[data]},
			Reflection3D[InfinitePlane[line[[1]] . basis, {axis, line[[2]] . basis}]]
		]
	]


(* ::Subsubsection::Closed:: *)
(*Polyhedron*)


PolyhedronSymmetry[
	(m_BoundaryMeshRegion | m_MeshRegion),
	asGroup_?BooleanQ, ops:OptionsPattern[]
] := With[{v = MeshCoordinates[m]},
	Block[{workingPrecision, distTolerance, g, axes, out},
		workingPrecision = ExtractWorkingPrecision[OptionValue[WorkingPrecision], v];
		distTolerance = ExtractTolerance[OptionValue[Tolerance], workingPrecision][[1]];
		g = MeshGraph[m];
		axes = SymmetryAxes[v, g, distTolerance];
		out = Map[PolyhedronSymmetry[m, v, g, asGroup, ops], axes];
		If[asGroup,
			PermutationGroup[Catenate[out]],
			TransformationGroup3D[Catenate[out]]
		]
	]
]

PolyhedronSymmetry[
	(m_BoundaryMeshRegion | m_MeshRegion),
	v_List, g_Graph, asGroup_?BooleanQ, ops:OptionsPattern[]
][basis_List] := With[{
		axis = basis[[1]] \[Cross] basis[[2]],
		centroid = Mean[v]
	},
	Block[{workingPrecision, angleTolerance, distTolerance, sameTest,
			ordering, back, \[CapitalGamma], S, o, k, axes
		},
		workingPrecision = ExtractWorkingPrecision[OptionValue[WorkingPrecision], v];
		{distTolerance, angleTolerance} = ExtractTolerance[
			OptionValue[Tolerance], workingPrecision
		];
		sameTest = PolyhedronSameTest[distTolerance, angleTolerance];
		
		{ordering, back} = PolyhedronOrder[
			m, v, g, axis, centroid, basis, distTolerance
		];
		{\[CapitalGamma], S} = PolyhedronEncode[v, g, axis, centroid, basis, ordering, back];
		o = Map[
			Length[#] / GCD[Length[#], GetCyclicOffset[#, #, sameTest]] &,
			S
		];
		k = Apply[GCD, o];
		
		axes = MapThread[
			PointSetReflection[1, workingPrecision, sameTest, Identity],
			{Map[First, \[CapitalGamma], {2}], S}
		];

		If[asGroup,
			With[{total = Length[axes]},
				axes = Flatten[axes, 1];
				axes = GatherBy[axes, First];
				axes = Select[axes, Length[#] == total&];
				axes = Map[#[[2]]&, axes, {2}];
			];
			Append[
				Map[PointSetReflectionCycle[ordering], axes],
				PointSetRotationCycle[k, ordering]
			],
			
			axes = Apply[Intersection, Map[First, axes, {2}]];
			Append[
				Map[PolyhedronReflectionTransform[centroid, axis, basis], axes],
				PolyhedronRotationTransform[axis, centroid, k]
			]
		]
	]
]


PolyhedronOrder[
	(m_BoundaryMeshRegion | m_MeshRegion),
	vn_List, g_Graph, axis_List, centroid_List,
	basis_List, tolerance_
] := Block[{cycle, back, \[CapitalGamma], line, i, j, k, pos},
	cycle = ConstantArray[0, Length[vn]];
	back = ConstantArray[0, Length[vn]];
	\[CapitalGamma] = ConstantArray[{}, Length[vn]];
	line = InfiniteLine[centroid, axis];
	
	i = Flatten@Position[vn, {x_, y_, z_} /; NormSqr[Normalize[{x, y, z}] - Normalize[axis]] <= tolerance^2 \[Or] NormSqr[Normalize[{x, y, z}] + Normalize[axis]] <= tolerance^2];
	If[i != {},
		cycle[[i]] = -1;
		j = AdjacencyList[g, First[i]];
		pos = basis . vn[[First[i]]];
		j = SortBy[j, ClockwiseSort[basis, vn, pos]];
		\[CapitalGamma][[1]] = j;
		cycle[[j]] = 1;
		back[[j]] = First[i],
		
		i = Position[MeshPrimitives[m, 1], x_Line /; RegionDistance[line, RegionCentroid[x]] <= tolerance, 1, 1];
		If[i != {},
			j = Flatten@Keys@Drop[ArrayRules[m["ConnectivityMatrix"[1, 0]][[i[[1, 1]]]]], -1];
			\[CapitalGamma][[1]] = j;
			cycle[[j]] = 1;
			back[[j[[1]]]] = j[[2]];
			back[[j[[2]]]] = j[[1]],
			
			i = Position[MeshPrimitives[m, 2], x_Polygon /; (pos = RegionIntersection[x, line]; \[Not](pos === EmptyRegion[3])), 1, 1];
			i = Flatten@Keys@Drop[ArrayRules[m["ConnectivityMatrix"[2, 0]][[i[[1, 1]]]]], -1];
			pos = If[ListQ[pos[[1, 1]]], pos[[1, 1]], pos[[1]]];
			pos = basis . pos;
			i = SortBy[i, ClockwiseSort[basis, vn, pos]];
			\[CapitalGamma][[1]] = i;
			cycle[[i]] = 1;
			back[[i]] = Prepend[i[[1 ;; -2]], i[[-1]]];
		]
	];
	
	For[k = 1, \[CapitalGamma][[k]] != {}, k++,
		Scan[PolyhedronOrderLoop1[vn, g, axis, centroid, k], \[CapitalGamma][[k]]]
	];
	{\[CapitalGamma][[;; k - 1]], back}
]

PolyhedronOrderLoop1[
	vn_List, g_Graph, axis_List, centroid_List, k_Integer
][i_Integer] := Block[{basis, pi, j},
	basis = LocalBasis[vn[[i]], centroid, axis];
	pi = basis . vn[[i]];
	j = SortBy[AdjacencyList[g, i], ClockwiseSort[basis, vn, pi]];
	Scan[PolyhedronOrderLoop2[vn, axis, centroid, k, i], j];
]

PolyhedronOrderLoop2[
	vn_List, axis_List, centroid_List, k_Integer, i_Integer
][j_Integer] :=
	If[cycle[[j]] == 0,
		AppendTo[\[CapitalGamma][[k + 1]], j];
		cycle[[j]] = k + 1;
		back[[j]] = i,
		
		If[cycle[[j]] == k + 1 \[And]
			Greater\[CapitalDelta][vn[[back[[j]]]], vn[[i]], vn[[j]], axis, centroid],
			
			\[CapitalGamma][[k + 1]] = DeleteCases[\[CapitalGamma][[k + 1]], j];
			AppendTo[\[CapitalGamma][[k + 1]], j];
			back[[j]] = i
		]
	]

LocalBasis[point_List, centroid_List, axis_List] := With[{
		basis = Orthogonalize[{point - centroid, axis}]
	},
	{basis[[2]], basis[[2]] \[Cross] basis[[1]]}
]

ClockwiseSort[basis_List, vn_List, pos_List][i_Integer] :=
	Apply[ArcTan, basis . vn[[i]] - pos]

Greater\[CapitalDelta][a_List, b_List, point_List, axis_List, centroid_List] := With[{
		basisY = centroid - point,
		diff = a - b
	},
	Block[{val = (basisY \[Cross] axis) . diff},
		If[val != 0, val > 0,
			val = basisY . diff;
			If[val != 0, val > 0,
				axis . diff > 0
			]
		]
	]
]


PolyhedronEncode[
	vn_List, g_Graph, axis_List, centroid_List, basis_List, ordering_List, back_List
] := With[{
		\[CapitalGamma] = Map[
			PolyhedronEncodeElement[vn, g, axis, centroid, basis, back],
			ordering, {2}
		]
	}, Block[{S},
		S = Map[RelativeAngles, \[CapitalGamma]];
		{\[CapitalGamma], S}
	]
]

PolyhedronEncodeElement[
	vn_List, g_Graph, axis_List, centroid_List, basis_List, back_List
][i_Integer] := Block[{localBasis, pi, adjacent},
	localBasis = LocalBasis[vn[[i]], centroid, axis];
	pi = localBasis . vn[[i]];
	adjacent = AdjacencyList[g, i];
	adjacent = SortBy[adjacent, ClockwiseSort[localBasis, vn, pi]];
	adjacent = RotateLeft[adjacent, Position[adjacent, back[[i]]][[1, 1]] - 1];
	adjacent = Map[\[CapitalDelta][vn[[#]], vn[[i]], axis, centroid]&, adjacent];
	{
		ArcTan[basis[[1]] . vn[[i]], basis[[2]] . vn[[i]]],
		Norm[vn[[i]] - centroid],
		axis . (vn[[i]] - centroid),
		adjacent
	}
]

RelativeAngles[\[CapitalGamma]_List] := With[{n = Length[\[CapitalGamma]]},
	Table[
		Prepend[\[CapitalGamma][[j, 2;;]], Mod[\[CapitalGamma][[Mod[j + 1, n, 1], 1]] - \[CapitalGamma][[j, 1]], 2\[Pi]]],
		{j, n}
	]
]

\[CapitalDelta][a_List, point_List, axis_List, centroid_List] := With[{
		basisY = centroid - point
	},
	{basisY \[Cross] axis, basisY, axis} . (a - point)
]


(* 
	pre: distTolerance and angleTolerance are small numbers
	ret: test for the equality of tuples encoded according to PolyhedronEncode
*)
PolyhedronSameTest[0, 0] := Equal
PolyhedronSameTest[distTolerance_, angleTolerance_][a_List, b_List] :=
	Abs[a[[1]] - b[[1]]] <= angleTolerance \[And]
	Abs[a[[2]] - b[[2]]] <= distTolerance \[And]
	Abs[a[[3]] - b[[3]]] <= distTolerance \[And]
	ListEqual[
		a[[4]], b[[4]], Length[a[[4]]],
		EncodedAdjacentSameTest[distTolerance]
	]

EncodedAdjacentSameTest[tolerance_][a_List, b_List] :=
	 Abs[a[[1]] - b[[1]]] <= tolerance \[And]
	 Abs[a[[2]] - b[[2]]] <= tolerance


PolyhedronRotationTransform[axis_List, centroid_List, k_Integer] :=
	If[k == 1,
		Nothing,
		Rotation3D[InfiniteLine[centroid, axis], 2\[Pi] / k]
	]

PolyhedronReflectionTransform[centroid_List, axis_List, basis_List][angle_] :=
	Reflection3D[
		InfinitePlane[centroid, {axis, FromPolarCoordinates[{1, angle}] . basis}]
	]


(* ::Subsection::Closed:: *)
(*TransformationGroup*)


ToTransformations[permutation_Cycles, coordinates_List] := 
	If[permutation == Cycles[{}], Nothing,
	With[{
			order = PermutationOrder[permutation],
			cycle = coordinates[[permutation[[1, 1]]]]
		},
		If[RotationQ[permutation, coordinates],
			(* Rotation *)
			If[Length[coordinates[[1]]] == 2,
				(* 2D *)
				Rotation[Mean[cycle], 2\[Pi] / order],
				(* 3D *)
				Block[{longCycle, axis},
					longCycle = Select[First[permutation], Length[#] >= 3&];
					longCycle = coordinates[[longCycle[[{1, 2, 3}]]]];
					axis = (longCycle[[2]] - longCycle[[1]])
						 \[Cross] (longCycle[[3]] - longCycle[[1]]);
					Rotation3D[InfiniteLine[Mean[cycle], axis], 2\[Pi] / order]
				]
			],
			(* Reflection *)
			With[{centroid = Mean@coordinates[[PermutationSupport[permutation]]]},
				If[Length[coordinates[[1]]] == 2,
					(* 2D *)
					Reflection@InfiniteLine[
						centroid, Cross[cycle[[1]] - cycle[[2]]]
					],
					(* 3D *)
					Reflection3D@InfinitePlane[centroid, {}]
				]
			]
		]
	]
]

RotationQ[permutation_Cycles, coordinates_List] := With[{
		centroid = Mean@coordinates[[PermutationSupport[permutation]]]
	},
	If[PermutationOrder[permutation] != 2, True,
		If[AnyTrue[First[permutation], Mean@coordinates[[#]] != centroid&], False,
			True
		]
	]
]


(* ::Subsubsection::Closed:: *)
(*TransformationGroup*)


TransformationGroupGenerators[group_TransformationGroup] := group[[1]]

TransformationGroupElements[group_TransformationGroup] := With[{
		gen = TransformationGroupGenerators[group]
	},
	Block[{rot, refl, transform},
		rot = Cases[gen, _Rotation];
		If[rot == {}, Return[gen]];
		rot = Fold[TransformationSpan, rot];
		refl = TransformationSpan[Cases[gen, _Reflection], rot];
		Append[refl, rot]
	]
]

RotationAngle[rot_Rotation] := rot[[2]]
RotationPoint[rot_Rotation] := Point[rot[[1]]]

ReflectionPoint[refl_Reflection] := Point[refl[[1, 1]]]
ReflectionNormal[refl_Reflection] := Cross[refl[[1, 2]]]
ReflectionLine[refl_Reflection] := refl[[1]]

TransformationPrimitive[rot_Rotation] := {
	RotationPoint[rot],
	Text[
		RotationAngle[rot],
		First@RotationPoint[rot], {0, -3/2},
		Background -> LightYellow
	]
}
TransformationPrimitive[refl_Reflection] :=
	ReflectionLine[refl]
TransformationPrimitive[group_TransformationGroup] :=
	Map[TransformationPrimitive, TransformationGroupElements[group]]

Rotation /: RotationTransform[rot_Rotation] :=
	RotationTransform[RotationAngle[rot], First@RotationPoint[rot]]
Reflection /: ReflectionTransform[refl_Reflection] :=
	ReflectionTransform[ReflectionNormal[refl], First@ReflectionPoint[refl]]

Rotation /: TransformationFunction[rot_Rotation] := RotationTransform[rot]
Reflection /: TransformationFunction[refl_Reflection] := ReflectionTransform[refl]
TransformationGroup /: TransformationFunction[group_TransformationGroup] :=
	Map[TransformationFunction, TransformationGroupGenerators[group]]

TransformationSpan[rot1_Rotation, rot2_Rotation] := (
	If[RotationPoint[rot1] != RotationPoint[rot2],
		Throw["Cannot combine transformations around different points!"]];
	If[RotationAngle[rot1] == RotationAngle[rot2],
		Return[rot1]
	];
	Rotation[rot1[[1]], Abs[rot1[[2]] - rot2[[2]]]]
)

TransformationProduct[refl_Reflection, transform_Reflection] :=
	Reflection[TransformationFunction[transform][refl[[1]]]]

TransformationProduct[rot_Rotation][refl_Reflection] :=
	Simplify@Reflection@RotationTransform[
		RotationAngle[rot] / 2, First@RotationPoint[rot]][refl[[1]]
	]
	
TransformationSpan[refl_List, rot_Rotation] :=
	FixedPoint[RotateIteration[rot], refl, 2\[Pi] / RotationAngle[rot]]

RotateIteration[rot_Rotation][refl_List] :=
	Union[refl, Map[TransformationProduct[rot], refl]]


(* ::Subsubsection::Closed:: *)
(*TransformationGroup3D*)


TransformationGroupGenerators[group_TransformationGroup3D] := group[[1]]

TransformationGroupElements[group_TransformationGroup3D] :=
	TransformationGroupGenerators[group]

RotationAngle[rot_Rotation3D] := rot[[2]]
RotationAxis[rot_Rotation3D] := rot[[1]]

ReflectionPoint[refl_Reflection3D] := Point[refl[[1, 1]]]
ReflectionNormal[refl_Reflection3D] := Apply[Cross, refl[[1, 2]]]
ReflectionPlane[refl_Reflection3D] := refl[[1]]

TransformationPrimitive[rot_Rotation3D] := Nothing
TransformationPrimitive[refl_Reflection3D] := ReflectionPlane[refl]
TransformationPrimitive[group_TransformationGroup3D] :=
	Map[TransformationPrimitive, TransformationGroupElements[group]]

Rotation3D /: RotationTransform[rot_Rotation3D] :=
	RotationTransform[
		RotationAngle[rot], RotationAxis[rot][[1]], RotationAxis[rot][[2]]
	]
Reflection3D /: ReflectionTransform[refl_Reflection3D] :=
	ReflectionTransform[ReflectionNormal[refl], First@ReflectionPoint[refl]]

Rotation3D /: TransformationFunction[rot_Rotation3D] :=
	RotationTransform[rot]
Reflection3D /: TransformationFunction[refl_Reflection3D] :=
	ReflectionTransform[refl]
TransformationGroup3D /: TransformationFunction[group_TransformationGroup3D] :=
	Map[TransformationFunction, TransformationGroupGenerators[group]]


(* ::Subsection::Closed:: *)
(*End the package*)


End[ ];


EndPackage[ ]
