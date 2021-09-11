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

// History:
 2021 04 19 - ET - Unit implementation Start's.
}

unit OpenArchWall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Dialogs,
  //GLscene:
  GLScene, GLObjects, OpenGLAdapter, GL,
  //OpenArch:
  OpenArchTranslate, OpenArchTypes, OpenArchLine,
  OpenArchVectorGeometry,
  //GLScene:
  GLVectorGeometry, GLCoordinates, GLGeomObjects, GLVectorTypes,
  GLColor, GLMultiPolygon;

type

  { TArch2DWall }
  TArch2DWall= class(TArchEntity)
    private
    public
      constructor Create(AOwner: TComponent); override;
      procedure Update(AVertices: TArch3DPointArray);
  end;

  { TArch3DWall }

  TArch3DWall= class(TArchEntity)
    private
    public
      constructor Create(AOwner: TComponent); override;
      procedure Update(AVertices: TArch3DPointArray; AHeight: TGLfloat);
      procedure UpdateBaseFace(AVertices: TArch3DPointArray);
      procedure UpdateStartFace(AVertices: TArch3DPointArray; AHeight: TGLfloat);
      procedure UpdateEndFace(AVertices: TArch3DPointArray; AHeight: TGLfloat);
      procedure UpdateLeftFace(AVertices: TArch3DPointArray; AHeight: TGLfloat);
      procedure UpdateRightFace(AVertices: TArch3DPointArray; AHeight: TGLfloat);
      procedure UpdateTopFace(AVertices: TArch3DPointArray; AHeight: TGLfloat);
      function AngleFace(AVertices: TArch3DPointArray): single;
  end;

  { TArchWall }
  TArchWall = class(TArchEntity)
    private
      FWallID:string;
      FWallState: TArchWallState;
      FSelected: Boolean;
      FStartWall,
      FEndWall: IArch3DPoint;
      FRefLinePosition: TArchWallRefLinePosition;
      FWallHeight,
      FWallThickness,
      FBaseOffset,
      FTopOffset:TGLfloat;
      function GetWall2D: TArch2DWall;
      function GetWall3D: TArch3DWall;
      procedure SetSelected(AValue: Boolean);
      function GetReferenceWallLine: TArchReferenceWallLine;
      function GetMakeWallLine: TArchMakeWallLine;
      procedure DelMakeWallLine();
      function GetWallVectorDirector():IArch3DPoint;
      function GetVertices(const VP: IArch3DPoint):TArch3DPointArray;
      procedure Add2dWall();
      procedure Add3dWall();
    public
      procedure Clone(AClonedWall:TArchWall);
      procedure CreateReferenceline();
      procedure ShowReferenceline();
      procedure HideReferenceLine();
      procedure StartingWall(AMessage: TArchMessage);
      procedure FinishingWall(AMessage:TArchMessage);
      procedure Update();
      constructor Create(AOwner: TComponent; AMessage: TArchMessage); overload;
      property Selected:Boolean read FSelected write SetSelected;
      property Wall2D:TArch2DWall read GetWall2D;
      property Wall3D:TArch3DWall read GetWall3D;
  end;

  { TArchWalls}
  TArchWalls = class(TArchEntity)
  private
    FStatus: TArchProjectState;
    FSelectedWalls: array of Integer;
    FElevationBase,
    FHeightBase: Double;
    procedure SetStatus(AValue: TArchProjectState);
    function GetMessageToSend(aState: TArchProjectState; aMessage:TArchMessage):
      TArchMessage;
    function FindWallByID(AWallID:String):TArchWall;
  public
    constructor Create(AOwner: TComponent; AEnviroment:TArchEnviromentType); overload;
    function GetElevationBase:Double;
    function GetHeightBase:Double;
    procedure CreateWall(AMessage: TArchMessage);
    procedure StartingWall(AMessage: TArchMessage);
    procedure FinishingWall(AMessage: TArchMessage);
    procedure AddSelectedWall(const AIndex:integer);
    function ChangeState(AMessage:TArchMessage):TArchMessage;
    procedure DeselectAllWalls();
    procedure SselectAllWalls();
    procedure SelectWall(AIndex:Integer);
    procedure Update(AWalls:TArchWalls; AMessage: TArchMessage);
    procedure AddWall(AWall: TArchWall);
    function CheckWallExists(const AWallID:string; var vWallPosition: integer): Boolean;
    property Status:TArchProjectState read FStatus write SetStatus;
  end;


implementation
uses
  OpenArchSysParameters, OpenArchStory;

//-----------------------------------------------------------------//
{ TArch3DWall }
//-----------------------------------------------------------------//
constructor TArch3DWall.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,ARCH_WALL3D);
  {Adding the Base Polygon: index = 0 }
  AddChild(TGLMultiPolygon.Create(Self));
  {Adding the Wall Start Polygon: index = 1}
  AddChild(TGLMultiPolygon.Create(Self));
  {Adding the Wall End Polygon: index = 2}
  AddChild(TGLMultiPolygon.Create(Self));
  {Adding The Wall Left Side Polygon: index = 3}
  AddChild(TGLMultiPolygon.Create(Self));
  {Adding the Wall Right Side Polygon: index = 4}
  AddChild(TGLMultiPolygon.Create(Self));
  {Adding the Wall Top Polygon: index = 5}
  AddChild(TGLMultiPolygon.Create(Self));
end;

procedure TArch3DWall.Update(AVertices: TArch3DPointArray; AHeight: TGLfloat);
begin
  UpdateBaseFace(AVertices);
  UpdateStartFace(AVertices,AHeight);
  UpdateEndFace(AVertices,AHeight);
  UpdateLeftFace(AVertices, AHeight);
  UpdateRightFace(AVertices, AHeight);
  UpdateTopFace(AVertices, AHeight);

  Visible:=True;
  StructureChanged;
end;

procedure TArch3DWall.UpdateBaseFace(AVertices: TArch3DPointArray);
var
  i:integer;
begin
 with Children[0] as TGLMultiPolygon do
 begin
   Material.FrontProperties.Diffuse.Color:= clrRed;
   Contours.Clear;
   Contours.Add;
   with Contours.Items[0].Nodes do
   begin
     Clear;
     for i:=0 to (Length(AVertices)-1) do
     begin
       AddNode(Avertices[i].GLCoordinatesMake(Self));
     end;
   end;
 end;
end;

procedure TArch3DWall.UpdateStartFace(AVertices: TArch3DPointArray;
  AHeight: TGLfloat);
var
  i:integer;
  startFaceVertices: TArch3DPointArray;
begin
  with Children[1] as TGLMultiPolygon do
  begin
   Position.SetPoint(AVertices[0].Vector3FMake);
   PitchAngle:=-90;
   TurnAngle:= AngleFace(AVertices)-90;
   SetLength(startFaceVertices,4);
   startFaceVertices[3]:= TArch3DPoint.New(0,
                                           AHeight,
                                           0);
   startFaceVertices[2]:= TArch3DPoint.New(AVertices[0].DistanceTo(AVertices[3]),
                                           AHeight,
                                           0);
   startFaceVertices[1]:= TArch3DPoint.New(AVertices[0].DistanceTo(AVertices[3]),
                                           0,
                                           0);
   startFaceVertices[0]:= TArch3DPoint.New(0,
                                           0,
                                           0);
   Material.FrontProperties.Diffuse.Color:= clr3DLight;
   Contours.Clear;
   Contours.Add;
   with Contours.Items[0].Nodes do
   begin
     Clear;
     for i:=0 to (Length(startFaceVertices)-1) do
     begin
       AddNode(startFaceVertices[i].GLCoordinatesMake(Self));
     end;
   end;
  end;
end;

procedure TArch3DWall.UpdateEndFace(AVertices: TArch3DPointArray;
  AHeight: TGLfloat);
var
  i:integer;
  endFaceVertices: TArch3DPointArray;
begin
 with Children[2] as TGLMultiPolygon do
 begin
   Position.SetPoint(AVertices[1].Vector3FMake);
   PitchAngle:=-90;
   TurnAngle:= AngleFace(AVertices)-90;
   SetLength(endFaceVertices,4);
   endFaceVertices[3]:= TArch3DPoint.New(0,
                                           AHeight,
                                           0);
   endFaceVertices[2]:= TArch3DPoint.New(AVertices[0].DistanceTo(AVertices[3]),
                                           AHeight,
                                           0);
   endFaceVertices[1]:= TArch3DPoint.New(AVertices[0].DistanceTo(AVertices[3]),
                                           0,
                                           0);
   endFaceVertices[0]:= TArch3DPoint.New(0,
                                           0,
                                           0);
   Material.FrontProperties.Diffuse.Color:= clr3DLight;
   Contours.Clear;
   Contours.Add;
   with Contours.Items[0].Nodes do
   begin
     Clear;
     for i:=0 to (Length(endFaceVertices)-1) do
     begin
       AddNode(endFaceVertices[i].GLCoordinatesMake(Self));
     end;
   end;
 end;
end;

procedure TArch3DWall.UpdateLeftFace(AVertices: TArch3DPointArray;
  AHeight: TGLfloat);
var
  i:integer;
  leftFaceVertices: TArch3DPointArray;
begin
 with Children[3] as TGLMultiPolygon do
 begin
   Position.SetPoint(AVertices[3].Vector3FMake);
   PitchAngle:=-90;
   TurnAngle:= AngleFace(AVertices)-180;
   SetLength(leftFaceVertices,4);
   leftFaceVertices[3]:= TArch3DPoint.New(0,
                                          AHeight,
                                          0);
   leftFaceVertices[2]:= TArch3DPoint.New(AVertices[0].DistanceTo(AVertices[1]),
                                           AHeight,
                                           0);
   leftFaceVertices[1]:= TArch3DPoint.New(AVertices[0].DistanceTo(AVertices[1]),
                                           0,
                                           0);
   leftFaceVertices[0]:= TArch3DPoint.New(0,
                                           0,
                                           0);
   Material.FrontProperties.Diffuse.Color:= clrYellow;
   Contours.Clear;
   Contours.Add;
   with Contours.Items[0].Nodes do
   begin
     Clear;
     for i:=0 to (Length(leftFaceVertices)-1) do
     begin
       AddNode(leftFaceVertices[i].GLCoordinatesMake(Self));
     end;
   end;
 end;
end;

procedure TArch3DWall.UpdateRightFace(AVertices: TArch3DPointArray;
  AHeight: TGLfloat);
var
  i:integer;
  rightFaceVertices: TArch3DPointArray;
begin
  with Children[4] as TGLMultiPolygon do
  begin
    Position.SetPoint(AVertices[0].Vector3FMake);
    PitchAngle:=-90;
    TurnAngle:= AngleFace(AVertices)-180;
    SetLength(rightFaceVertices,4);
    rightFaceVertices[3]:= TArch3DPoint.New(0,
                                          AHeight,
                                          0);
    rightFaceVertices[2]:= TArch3DPoint.New(AVertices[0].DistanceTo(AVertices[1]),
                                           AHeight,
                                           0);
    rightFaceVertices[1]:= TArch3DPoint.New(AVertices[0].DistanceTo(AVertices[1]),
                                           0,
                                           0);
    rightFaceVertices[0]:= TArch3DPoint.New(0,
                                           0,
                                           0);
    Material.FrontProperties.Diffuse.Color:= clrYellow;
    Contours.Clear;
    Contours.Add;
    with Contours.Items[0].Nodes do
    begin
     Clear;
     for i:=0 to (Length(rightFaceVertices)-1) do
     begin
       AddNode(rightFaceVertices[i].GLCoordinatesMake(Self));
     end;
    end;
  end;
end;

procedure TArch3DWall.UpdateTopFace(AVertices: TArch3DPointArray;
  AHeight: TGLfloat);
var
  i:integer;
  topFaceVertices: TArch3DPointArray;
begin
  with Children[5] as TGLMultiPolygon do
  begin
    topFaceVertices:= AVertices;
    Material.FrontProperties.Diffuse.Color:= clrRed;
    Contours.Clear;
    Contours.Add;
    with Contours.Items[0].Nodes do
    begin
      Clear;
      for i:=0 to (Length(topFaceVertices)-1) do
      begin
        topFaceVertices[i].SetValue(2, (topFaceVertices[i].Z + AHeight));
        AddNode(topFaceVertices[i].GLCoordinatesMake(Self));
      end;
   end;
  end;
end;

function TArch3DWall.AngleFace(AVertices: TArch3DPointArray): single;
var
  vAngle: single;
begin
  vAngle:=(AVertices[0].VectorDirectorTo(AVertices[1])).AngleTo(TArch3DPoint.New(1,0,0));
  if (AVertices[0].X < AVertices[1].X) and (AVertices[0].Y < AVertices[1].Y) then
  begin
    Result:= -vAngle;
  end;
  if (AVertices[0].X < AVertices[1].X) and (AVertices[0].Y >= AVertices[1].Y) then
  begin
    Result:= vAngle;
  end;
  if (AVertices[0].X >= AVertices[1].X) and (AVertices[0].Y < AVertices[1].Y) then
  begin
    Result:= -vAngle;
  end;
  if (AVertices[0].X >= AVertices[1].X) and (AVertices[0].Y >= AVertices[1].Y) then
  begin
    Result:= vAngle;
  end;
end;


//-----------------------------------------------------------------//
{ TArch2DWall }
//-----------------------------------------------------------------//
constructor TArch2DWall.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, ARCH_WALL2D);
  AddChild(TGLPolygon.Create(Self));
end;

procedure TArch2DWall.Update(AVertices: TArch3DPointArray);
var
  i:integer;
begin
  with Children[0] as TGLPolygon do
  begin
    Nodes.Clear;
    Material.FrontProperties.Diffuse.Color:= clrRed;
    for i:=0 to (Length(AVertices)-1) do
    begin
      AddNode(Avertices[i].X,
              Avertices[i].Y,
              Avertices[i].Z);
    end;
  end;
  Visible:=True;
end;

//-----------------------------------------------------------------//
{ TArchWall }
//-----------------------------------------------------------------//
procedure TArchWall.SetSelected(AValue: Boolean);
begin
  FSelected:=AValue;
  if AValue then ShowReferenceline()
    else HideReferenceLine();
end;

function TArchWall.GetWall2D: TArch2DWall;
var
  i: integer;
  entity: TArchEntity;
begin
  i:=0;
  entity:= Children[i] as TArchEntity;
  while entity.EntityName <> ARCH_WALL2D do
  begin
    i:= i+1;
    entity:= Children[i] as TArchEntity;
  end;
  Result:= entity as TArch2DWall;
end;

function TArchWall.GetWall3D: TArch3DWall;
var
  i: integer;
  entity: TArchEntity;
begin
  i:=0;
  entity:= Children[i] as TArchEntity;
  while entity.EntityName <> ARCH_WALL3D do
  begin
    i:= i+1;
    entity:= Children[i] as TArchEntity;
  end;
  Result:= entity as TArch3DWall;
end;

function TArchWall.GetReferenceWallLine: TArchReferenceWallLine;
var
  i: integer;
  entity: TArchEntity;
begin
  i:=0;
  entity:= Children[i] as TArchEntity;
  while entity.EntityName <> ARCH_REFERENCE_LINE do
  begin
    i:= i+1;
    entity:= Children[i] as TArchEntity;
  end;
  Result:= entity as TArchReferenceWallLine;
end;

function TArchWall.GetMakeWallLine: TArchMakeWallLine;
var
  i: integer;
  entity: TArchEntity;
begin
  i:=0;
  entity:= Children[i] as TArchEntity;
  while entity.EntityName <> ARCH_MAKE_LINE do
  begin
    i:= i+1;
    entity:= Children[i] as TArchEntity;
  end;
  Result:= entity as TArchMakeWallLine;
end;

function TArchWall.GetVertices(const VP: IArch3DPoint): TArch3DPointArray;
begin
  SetLength(Result,4);
  case FRefLinePosition of
    rlOutiside :
      begin
        Result[0]:= FStartWall.ProjectedPoint(FWallThickness,VP.VectorNegate);
        Result[1]:= FEndWall.ProjectedPoint(FWallThickness,VP.VectorNegate);
        Result[2]:= FEndWall;
        Result[3]:= FStartWall;
      end;
    rlCenter :
      begin
        Result[0]:= FStartWall.ProjectedPoint(FWallThickness/2,VP.VectorNegate);
        Result[1]:= FEndWall.ProjectedPoint(FWallThickness/2,VP.VectorNegate);
        Result[2]:= FEndWall.ProjectedPoint(FWallThickness/2,VP);
        Result[3]:= FStartWall.ProjectedPoint(FWallThickness/2,VP);
      end;
    rlInside :
      begin
        Result[0]:= FStartWall;
        Result[1]:= FEndWall;
        Result[2]:= FEndWall.ProjectedPoint(FWallThickness,VP);
        Result[3]:= FStartWall.ProjectedPoint(FWallThickness,VP);
      end;
  end;
end;

procedure TArchWall.Add2dWall();
begin
  AddChild(TArch2DWall.Create(Self));
  with Wall2D do
  begin
    Visible:=False;
  end;
end;

procedure TArchWall.Add3dWall();
begin
   AddChild(TArch3DWall.Create(Self));
   with Wall3D do
   begin
     Visible:=False;
   end;
end;

procedure TArchWall.Clone(AClonedWall: TArchWall);
begin
  if Assigned(AClonedWall) then
  begin
    Self.FWallID:=          TArchWall(AClonedWall).FWallID;
    Self.FWallState:=       TArchWall(AClonedWall).FWallState;
    Self.FStartWall:=       TArchWall(AClonedWall).FStartWall;
    Self.FEndWall:=         TArchWall(AClonedWall).FEndWall;
    Self.FRefLinePosition:= TArchWall(AClonedWall).FRefLinePosition;
    Self.FWallHeight:=      TArchWall(AClonedWall).FWallHeight;
    Self.FWallThickness:=   TArchWall(AClonedWall).FWallThickness;
    Self.FBaseOffset:=      TArchWall(AClonedWall).FBaseOffset;
    Self.FTopOffset:=       TArchWall(AClonedWall).FTopOffset;
  end;
end;

procedure TArchWall.DelMakeWallLine();
var
  i: integer;
  entity: TArchEntity;
begin
  i:=0;
  entity:= Children[i] as TArchEntity;
  while entity.EntityName <> ARCH_MAKE_LINE do
  begin
    i:= i+1;
    entity:= Children[i] as TArchEntity;
  end;
  Remove(entity, False);
end;

function TArchWall.GetWallVectorDirector(): IArch3DPoint;
begin
  Result:= TArch3DPoint.New(FStartWall.VectorDirectorTo(FEndWall))
end;

procedure TArchWall.CreateReferenceline();
begin
  AddChild(TArchReferenceWallLine.Create(Self, FStartWall, FEndWall));
  HideReferenceLine();
end;

procedure TArchWall.ShowReferenceline();
var
  refLine: TArchReferenceWallLine;
begin
  refLine:= GetReferenceWallLine;
  refLine.Visible:=true;
end;

procedure TArchWall.HideReferenceLine();
var
  refLine: TArchReferenceWallLine;
begin
  refLine:= GetReferenceWallLine;
  refLine.Visible:=false;
end;

procedure TArchWall.StartingWall(AMessage: TArchMessage);
var
  refLine: TArchReferenceWallLine;
  mkLine: TArchMakeWallLine;
begin
  with FEndWall do
    begin
      SetVector(AMessage[2].ParamCIValue.Coordinates2D.X,
                AMessage[2].ParamCIValue.Coordinates2D.Y,
                AMessage[2].ParamCIValue.Coordinates2D.Z);
    end;
  refLine:= GetReferenceWallLine;
  refLine.StartingWall(FStartWall,FEndWall);
  mkLine:= GetMakeWallLine;
  mkLine.StartingWall(GetVertices(GetWallVectorDirector().VectorPerpendicular()));
  SetSelected(True);
  StructureChanged;
end;

procedure TArchWall.FinishingWall(AMessage: TArchMessage);
begin
  with FEndWall do
    begin
      SetVector(AMessage[2].ParamCIValue.Coordinates2D.X,
                AMessage[2].ParamCIValue.Coordinates2D.Y,
                AMessage[2].ParamCIValue.Coordinates2D.Z);
    end;
  DelMakeWallLine();
  SetSelected(False);
  if FEnviromentType= etFloorPlan then
    begin
      Wall2D.Update(GetVertices(GetWallVectorDirector().VectorPerpendicular()));
    end;
  if FEnviromentType= et3d then Wall3D.Update(GetVertices(GetWallVectorDirector().VectorPerpendicular()),FWallHeight);
end;

procedure TArchWall.Update();
begin
  case FEnviromentType of
    etFloorPlan:
      begin
        Wall2D.Update(GetVertices(GetWallVectorDirector().VectorPerpendicular()));
        Wall3D.Visible:=False;
      end;
    et3d:
      begin
        Wall3D.Update(GetVertices(GetWallVectorDirector().VectorPerpendicular()), FWallHeight);
        Wall2D.Visible:=False;
      end;
  end;
end;

constructor TArchWall.Create(AOwner: TComponent; AMessage: TArchMessage);
var
  vBaseLevel:Double;
begin
  inherited Create(AOwner, ARCH_WALL);
  vBaseLevel:= (AOwner as TArchWalls).GetElevationBase;
  FEnviromentType:= AMessage[3].ParamEVTValue;
  Position.SetPoint(0,0,vBaseLevel);
  FEndWall:=    TArch3DPoint.New(0,0,0);
  FStartWall:=  TArch3DPoint.New(0,0,0);
  FWallThickness:=  0.15;
  FBaseOffset:= 0;
  FTopOffset:=  0;
  FRefLinePosition:= rlCenter;
  FWallHeight:=3;
  with FStartWall do
    begin
      SetVector(AMessage[2].ParamCIValue.Coordinates2D.X,
                AMessage[2].ParamCIValue.Coordinates2D.Y,
                AMessage[2].ParamCIValue.Coordinates2D.Z);
    end;
  with FEndWall do
    begin
      SetVector(AMessage[2].ParamCIValue.Coordinates2D.X,
                AMessage[2].ParamCIValue.Coordinates2D.Y,
                AMessage[2].ParamCIValue.Coordinates2D.Z);
    end;

  CreateReferenceline();
  AddChild(TArchMakeWallLine.Create(Self));
  Add2dWall();
  Add3dWall();
  StructureChanged;
end;

{ TArchWalls }

procedure TArchWalls.SetStatus(AValue: TArchProjectState);
begin
  if FStatus= AValue then exit
  else FStatus:= AValue;
end;

function TArchWalls.GetMessageToSend(aState: TArchProjectState;
  aMessage: TArchMessage): TArchMessage;
var
  vWallSelected:TArchWall;
begin
  SetLength(Result,4);
  with Result[0] do
  begin
    ParamName:= ARCH_PROJECT_STATE;
    ParamType:= 5;
    ParamPSValue:= aState;
  end;
  with Result[1] do
  begin
    ParamName:= aMessage[1].ParamName;
    ParamType:= aMessage[1].ParamType;
    ParamPSValue:= aMessage[1].ParamPSValue;
  end;
  with Result[2] do
  begin
    ParamName:= aMessage[2].ParamName;
    ParamType:= aMessage[2].ParamType;
    ParamPSValue:= aMessage[2].ParamPSValue;
  end;
  with Result[3] do
  begin
    ParamName:= aMessage[3].ParamName;
    ParamType:= aMessage[3].ParamType;
    ParamEVTValue:= aMessage[3].ParamEVTValue;
  end;
  if aState= psFinishingElement then
  begin
    SetLength(Result,5);
    vWallSelected:= Children[FSelectedWalls[0]] as TArchWall;
    with Result[4] do
    begin
      ParamName:= ARCH_WALL_ID;
      ParamType:= 3;
      ParamStrValue:= vWallSelected.ID;
    end;
    vWallSelected:= Nil;
    FreeAndNil(vWallSelected);
  end;
end;

function TArchWalls.FindWallByID(AWallID: String): TArchWall;
var
  i: integer;
  vWall: TArchWall;
begin
  i:=0;
  if Count>i then
  begin
    vWall:= Children[i] as TArchWall;
    while vWall.ID <> AWallID do
      begin
        if i >= Count then Result:= nil
         else
           begin
             i:= i+1;
             vWall:= Children[i] as TArchWall;
           end;
      end;
      Result:= vWall;
  end else
  begin
    Result:= nil;
  end;
end;

constructor TArchWalls.Create(AOwner: TComponent;
  AEnviroment: TArchEnviromentType);
begin
  inherited Create(AOwner,ARCH_WALLS);
  FElevationBase:= (AOwner As TArchSceneStory).Elevation;
  FHeightBase:= (AOwner as TArchSceneStory).Height;
  FEnviromentType:=AEnviroment;
end;

function TArchWalls.GetElevationBase: Double;
begin
  Result:= FElevationBase;
end;

function TArchWalls.GetHeightBase: Double;
begin
  Result:= FHeightBase;
end;

procedure TArchWalls.CreateWall(AMessage:TArchMessage);
begin
  AddChild(TArchWall.Create(Self,AMessage));
  DeselectAllWalls();
  SelectWall(Count-1);
end;

procedure TArchWalls.StartingWall(AMessage: TArchMessage);
begin
  with Children[FSelectedWalls[0]] as TArchWall do
  begin
    StartingWall(AMessage);
  end;
end;

procedure TArchWalls.FinishingWall(AMessage: TArchMessage);
begin
  with Children[FSelectedWalls[0]] as TArchWall do
  begin
    FinishingWall(AMessage);
  end;
end;

procedure TArchWalls.AddSelectedWall(const AIndex: integer);
begin
  SetLength(FSelectedWalls,(Length(FSelectedWalls)+1));
  FSelectedWalls[Length(FSelectedWalls)]:= AIndex;
end;

{This function serves to transmit information by generic TArchMessage structure.
This informatio must be organized in the following order:
Parameter 0 = TArchProjectState - That indicates change of the states
Parameter 1 = TArchElementType - that indicates the element processed
Parameter 2 = TArchContextInfo - Containing information about the viewer
Parameter 3 = TArchEnviromentType - Containing the type of enviroment.
After finishing Element, add the ID Element Parameter:
Parameter 4 = Element ID (string)
----------
Here the management of the wall element is done:
}
function TArchWalls.ChangeState(AMessage: TArchMessage):TArchMessage;
begin
  if AMessage[1].ParamETValue= etWall then
  begin
    case AMessage[2].ParamCIValue.MouseEvent of
      meMouseDown :
        begin
          if AMessage[0].ParamPSValue = psNewElement then
          begin
            CreateWall(AMessage);
            Result:= GetMessageToSend(psStartingElement,AMessage);
          end;
          if AMessage[0].ParamPSValue= psStartingElement then
          begin
            FinishingWall(AMessage);
            Result:= GetMessageToSend(psFinishingElement,AMessage);
          end;
        end;
      meMouseMove:
        begin
          if AMessage[0].ParamPSValue= psStartingElement then
          begin
            StartingWall(AMessage);
            Result:=GetMessageToSend(psStartingElement,AMessage);
          end;
        end;
      else
        begin
          Result:= AMessage;
        end;
    end;
  end;
end;

procedure TArchWalls.DeselectAllWalls();
var
  i: integer;
  wall: TArchWall;
begin
  for i:=0 to Count-1 do
  begin
    wall:= Children[i] as TArchWall;
    wall.Selected:= False;
  end;
  SetLength(FSelectedWalls,0);
end;

procedure TArchWalls.SselectAllWalls();
var
  i: integer;
  wall: TArchWall;
begin
  for i:=0 to Count-1 do
  begin
    wall:= Children[i] as TArchWall;
    wall.Selected:= true;
    SetLength(FSelectedWalls, (Length(FSelectedWalls)+1));
    FSelectedWalls[Length(FSelectedWalls)]:= i;
  end;
end;

procedure TArchWalls.SelectWall(AIndex: Integer);
begin
  SetLength(FSelectedWalls, (Length(FSelectedWalls)+1));
  FSelectedWalls[Length(FSelectedWalls)-1]:= AIndex;
end;

procedure TArchWalls.Update(AWalls: TArchWalls; AMessage: TArchMessage);
var
  vWall:TArchWall;
  wallPosition: Integer;
  vMessage:TArchMessage;
begin
  { Checks if the wall already exists through the ID }
  if (CheckWallExists(AMessage[4].ParamStrValue,wallPosition)) then
  begin
    vWall:= Children[wallPosition] as TArchWall;
    vWall.Clone(AWalls.FindWallByID(AMessage[4].ParamStrValue));
    vWall.Update();
  end else
  begin
    vMessage:= AMessage;
    vMessage[3].ParamEVTValue:=FEnviromentType;
    AddChild(TArchWall.Create(Self,vMessage));
    vWall:=(Children[Count-1] as TArchWall);
    vWall.ID:=AMessage[4].ParamStrValue;
    vWall.Clone(AWalls.FindWallByID(AMessage[4].ParamStrValue));
    vWall.Update();
  end;
end;

procedure TArchWalls.AddWall(AWall: TArchWall);
begin

end;

function TArchWalls.CheckWallExists(const AWallID: string;
  var vWallPosition: integer): Boolean;
var
  i:integer;
  vWall:TArchWall;
begin
 if Count > 0 then
 begin
   for i:= 0 to Count-1 do
   begin
     vWall:=Children[i] as TArchWall;
     if (vWall.ID = AWallID) then
     begin
       vWallPosition:=i;
       Result:= True;
       Exit;
     end else   Result:= False;
   end;
 end else Result:= false;
end;

end.

