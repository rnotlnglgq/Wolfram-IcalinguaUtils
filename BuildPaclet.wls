#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory@If[$Notebooks, NotebookDirectory[], Directory[]]


paclet = CreatePacletArchive["Source/IcalinguaUtils", "Paclets"]


PacletManager`BuildPacletSiteFiles@Directory[]


PacletInstall[paclet, ForceVersionInstall -> True]


<< IcalinguaUtils`


ResetDirectory[]
