{---------------------------------------------------------------------------------
This file is part of OpenARCK Project.

   Copyright (C) 2021  Everton Teles
   email: info@evertonteles.com
   home: www.evertonteles.com

OpenArch is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
----------------------------------------------------------------------------------
 
This Package storage the classes to read the colors used by OpenARCK from XML file.
 
  Historic
  2016 MAY 25 -ET - TArckColorRGB Class Creted.
  2016 MAY 06 -ET - Implement RGB codes of the Basic, Grey, Ultraviolet and BlueMar8ne Palettea.
  2016 JAN 08 -ET - Choice the color tones that will work with OpenARCK
  
  
  TODO: Create a function to convert TArckColor in TColor
} 
 
Unit OpenArchColor;

interface

uses 
  { FreePascal: }
  Classes, Contnrs, LCLIntf, Graphics, DOM, Dialogs,
  
  { GLScene: }
  GLVectorGeometry, GLVectorTypes,
  //, Rtti, typinfo;
  
  { OpenARCK: }
  // Units:
  OpenArchIOFileA, OpenArchIOFile, OpenArchSysParameters;
 
type

//------------
{ TArchColor }
//~~~---------
TArchColor = Class
   private
  	// privete declarations
     FColorName : String;
     FRGBValue: TVector3f;
     function GetBValue: Single;
     function GetGLBValue: Single;
     function GetGLGValue: Single;
     function GetGLRGB: TVector3f;
     function GetGLRValue: Single;
     function GetGValue: Single;
     function GetRGB: TVector3f;
     function GetRValue: Single;
     procedure SetBValue(AValue: Single);
     procedure SetGValue(AValue: Single);
     procedure SetRValue(AValue: Single);
   public
     //converte string na RGB value da color
     function GetRGBInString(const AString:String) : TVector3f; Virtual;
     property RGBValue:TVector3f read GetRGB write FRGBValue;
     property glRGBValue:TVector3f read GetGLRGB;
   published
     property ColorName:String read FColorName write FColorName;
     property R:Single read GetRValue write SetRValue;
     property G:Single read GetGValue write SetGValue;
     property B:Single read GetBValue write SetBValue;
     property glR:Single read GetGLRValue;
     property glG:Single read GetGLGValue;
     property glB:Single read GetGLBValue;
   end;
 
//------------------
{ TArckColoPalette }
//------------------
TArchColorPalette = class
   private
     FColorPaletteName: String;
     FArchColorsList : TObjectList;
     FIOFile: TArchIOXMLFile;
     // Count colors of the palette
     function GetArchColorCount: Integer;
     // get color index in the list
     function GetArchColor(Index: Integer): TArchColor;

   public
     constructor Create(AFilePath:String);
     destructor Destroy; override;
     // get color by name
     function GetColorByName(AName: String): TArchColor;
     // add a Color to the list
     procedure AddArchColor(const AArchColor: TArchColor);

     //The number of Clors
     property Count : Integer read GeTArchColorCount;
     //Acces color by index
     property ArchColor[Index: Integer]: TArchColor read GeTArchColor;
     //Load ColorPalette and colors from XML file
     procedure LoadFronXML();
     procedure SaveData();
     //Create a color to add
     procedure ColorCreate(AName,ARGB:String);
   published
     // Name property
     property ColorPaletteName:String read FColorPaletteName write FColorPaletteName;
     // ColorsList
     property ColorsList:TObjectList read FArchColorsList write FArchColorsList;
   end;

var
  OPENARCHColorPalette,
  UserColorPalette: TArchColorPalette;
  
  
//--- 
implementation 

uses
  sysutils;

//------------
{ TArchColor }
//------------
//GetRGBInString
function TArchColor.GetRValue: Single;
begin
  Result:= FRGBValue.X;
end;

function TArchColor.GetBValue: Single;
begin
  Result:= FRGBValue.Z;
end;

function TArchColor.GetGLBValue: Single;
begin
  Result:= B/255;
end;

function TArchColor.GetGLGValue: Single;
begin
  Result:= G/255;
end;

function TArchColor.GetGLRGB: TVector3f;
begin
  Result:= Vector3fMake(glR,glG,glB);
end;

function TArchColor.GetGLRValue: Single;
begin
  Result:= R/255;
end;

function TArchColor.GetGValue: Single;
begin
  Result:= FRGBValue.Y;
end;

function TArchColor.GetRGB: TVector3f;
begin
  Result:= FRGBValue;
end;

procedure TArchColor.SetBValue(AValue: Single);
begin
  FRGBValue.Z:= AValue;
end;

procedure TArchColor.SetGValue(AValue: Single);
begin
  FRGBValue.Y:= AValue;
end;

procedure TArchColor.SetRValue(AValue: Single);
begin
  FRGBValue.X:= AValue;
end;

function TArchColor.GetRGBInString(const AString: String): TVector3f;
var
   values:TStringList;
begin
try
   values:= TStringList.Create;
   values.Delimiter:=',';
   values.DelimitedText:=AString;
   SetVector(Result,(StrToFloat(values[0])),
                    (StrToFloat(values[1])),
                    (StrToFloat(values[2])));
   values.free;
except
   ShowMessage('Valor RGB não convertido'+' '+'"'+AString+'"');
end;//close try
end;

//-------------------
{ TArchColorPalette }
//-------------------
//Get Color by Name
function TArchColorPalette.GetColorByName(AName: String): TArchColor;
var
i:integer;
vClr: TArchColor;
begin
  for i:= 0 to (FArchColorsList.Count-1) do
  begin
    vClr:= ArchColor[i];
    with vClr do
     begin
       if vClr.ColorName = AName then
       begin
       Result:= vClr;
       Exit;
       end;// close if
     end;// close with
  end;//close for
end;

// AddArckColor
procedure TArchColorPalette.AddArchColor(const AArchColor: TArchColor);
begin
  FArchColorsList.Add(AArchColor);
end;

// Create ColorPalette sending XML File path with parameter to parent class create method
constructor TArchColorPalette.Create(AFilePath:String);
begin
  FIOFile:= TArchIOXMLFile.New(AFilePath);
  // the list own the entities
  FArchColorsList:= TObjectList.Create(True);
end;

// Destroy
destructor TArchColorPalette.Destroy;
begin
  // frees entities
  FArchColorsList.Free;
  FIOFile.Free;
  inherited;
end;

// GeTArchColorRGBCount
function TArchColorPalette.GeTArchColorCount: Integer;
begin
  Result := FArchColorsList.Count;
end;

// GeTArchColoRGB
function TArchColorPalette.GeTArchColor(Index: Integer): TArchColor;
begin
  Result := TArchColor(FArchColorsList[Index]);
end;

//Load colors from XML File
procedure TArchColorPalette.LoadFronXML();
var
inode:TDOMNode;
clName,clRGB:String;
  procedure ProcessNode(Node: TDOMNode);
  var
  cnode: TDOMNode;
  begin
    if Node = Nil then Exit;
    with Node do
    begin
      if NodeName = 'ColorName' then clName:=Node.TextContent
        else if NodeName = 'RGB' then clRGB:=Node.TextContent;
    end;// close with
    // create color and add to palette
    if (clRGB <> '') then
    begin
      ColorCreate(clName,clRGB);
      clName:='';
      clRGB:='';
    end;
    cnode:= Node.FirstChild;
    //run every nodes and your child
    while cnode<> nil do
    begin
      ProcessNode(cnode);
      cnode:= cnode.NextSibling;
    end;
  end;
begin
  with FIOFile.Reader do
    begin;
      inode:= ArchXML.DocumentElement.FirstChild;
      while inode<> nil do
        begin
          ProcessNode(inode);
          inode:= inode.NextSibling;
        end;
    end;
end;

//Create and add color
procedure TArchColorPalette.ColorCreate(AName,ARGB:String);
var
color: TArchColor;
begin
color:=TArchColor.Create;
if Assigned(color) then
  begin
   color.ColorName:= AName;
   color.RgbValue:= color.GetRGBInString(ARGB);
   AddArchColor(color);
  end;
end;

procedure TArchColorPalette.SaveData;
var
  ParamList: TArchParamList;
begin
  // TO DO: Desenvolver algoritimo para salvar uma paleta de cor.
  //ParamList:= GetParamList;
  //FIOFile.SaveData(ParamList);
end;




//---
//---Inicializando A Paleta de Cores Padrão com o systema
//---
Initialization

//--Cria Paletas de Cores:
//OpenARCK :: Cores padrão do Sys
OPENARCHColorPalette:= TArchColorPalette.Create('Library OPENARCH/Colors/OPENARCHColorPallete.xml');
OPENARCHColorPalette.ColorPaletteName:='OPENARCHColorPalette';
//Carrega cores do XML para a paleta criada:
OPENARCHColorPalette.LoadFronXML;

//UserColorPalette : Cores com RGB pré definidos pelo usuário
UserColorPalette:= TArchColorPalette.Create('Library OPENARCH/Colors/UserColorPalette.xml');
UserColorPalette.ColorPaletteName:='UserColorPalette';
UserColorPalette.LoadFronXML;

//---
//Finaliza a Paleta de Cores com o Sistema
//---
Finalization

OPENARCHColorPalette.Free;
UserColorPalette.Free;

end.
