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
 2021 08 28 - ET - Unit implementation Start's.
}

unit OpenArchLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //GLScene:
  GLObjects, GLColor, GLCoordinates, GLVectorGeometry, GLVectorTypes,
  //Openarch:
  OpenArchTypes, OpenArchVectorGeometry, OpenArchGraphicResources;

type

  { TArchLines }
  TArchLines = class(TArchEntity)

  end;

  { TArchReferenceWallLine }
  TArchReferenceWallLine = class(TArchLines)
    private
    public
      constructor Create(AOwner: TComponent; AStartNode,AEndNode:IArch3DPoint); overload;
      procedure EditWall(AStartNode,AEndNode:IArch3DPoint);
      procedure StartingWall(AStartNode,AEndNode:IArch3DPoint);
  end;

  { TArchMakeWallLine }
  TArchMakeWallLine = class(TArchLines)
    private
    public
      constructor Create(AOwner: TComponent); override;
      procedure StartingWall(ANodes: TArch3DPointArray);
  end;

  { TArchLine }
  TArchLine = class(TArchLines)
    private
      FLineType:TArchLineType;
      FWidth: Single;
      FStartColor,
      FEndColor: TGLColor;
      FStartPoint,
      FEndPoint: TArch3DPoint;
      FStartOpacity,
      FEndOpacity: Single;
      function GetDirectorVector: IArch3DPoint;
      function GetLineTypeName: string;
      procedure UpdateNode;
      procedure SetGraphicAspects;
      function GetLenght:Single;
      function NumOfParts():integer;
      procedure DrawDash(aBegin,aEnd:TVector3f);
      procedure DrawDot(aBegin:TVector3f);
      procedure DrawGap(aBegin,aEnd:TVector3f);
      procedure AddGap(var restOfLenght:single;aGapIndex:integer);
      procedure AddDashed();
    public
      procedure ClearNodes;
      constructor Create(AOwner: TComponent); override;
      constructor Create(AOwner: TComponent; ALineType:TArchLineType); overload;
      procedure SetStarPoint(AX,AY,AZ:Single);
      procedure SetStartPoint(APoint:TArch3DPoint); overload;
      procedure SetEndPoint(AX,AY,AZ: Single);
      procedure SetEndPoint(APoint: TArch3DPoint);
      procedure SetLineType(ALineType:TArchLineType);
      property LineLenght:Single read GetLenght;
      property LineDirectorVector:IArch3DPoint read GetDirectorVector;
      property LineTypeName:string read GetLineTypeName;
      property LineType:TArchLineType read FLineType write SetLineType;
  end;

implementation
uses
  OpenArchSysParameters;

{ TArchLine }

procedure TArchLine.UpdateNode;
begin
  case FLineType.LineStyle of
    lsSolid    :
      begin
        ClearNodes;
        DrawDash(FStartPoint.Vector3FMake,FEndPoint.Vector3FMake);
      end;
    lsDashed   :
      begin
        ClearNodes;
        AddDashed();
      end;
    lsSimbolic : ;// to develop
  end;
  StructureChanged;
end;

function TArchLine.GetDirectorVector: IArch3DPoint;
begin
  Result:= FEndPoint.VectorDirectorTo(FStartPoint);
end;

function TArchLine.GetLineTypeName: string;
begin
  Result:= FLineType.LineTypeName;
end;




// Create the nodes of the line:
procedure TArchLine.SetGraphicAspects;
var
  GLLine: TGLLines;
  GLPoints: TGLPoints;
begin
  GLLine:= Children[1] as TGLLines;
  with GLLine do
  begin
    NodesAspect:=lnaInvisible;
    ShowAxes:=false;
    LineColor.SetColor(0,0,0,0);
    Options:=[loUseNodeColorForLines];
    NodeColor.SetColor(FStartColor.Red,FStartColor.Green,FStartColor.Blue,FStartColor.Alpha);
    LineWidth:=FWidth;
  end;
  GLPoints:= Children[0] as TGLPoints;
  with GLPoints do
  begin
    Size:= FWidth*1;
    Style:= psSquare;
    Static:=True;
    Colors.Add(0,0,0,1);
  end;
end;

constructor TArchLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,ARCH_LINE);
  FStartPoint:= TArch3DPoint.Create(0,0,0);
  FEndPoint:= TArch3DPoint.Create(0,0,0);
  FStartOpacity:=1;
  FEndOpacity:=1;
  FWidth:=1.45;
  FStartColor:= TGLColor.Create(Self);
  FEndColor:= TGLColor.Create(Self);
  FStartColor.SetColor(0,0,0,FStartOpacity);
  FEndColor.SetColor(0,0,0,FEndOpacity);
  AddChild(TGLPoints.Create(Self));
  AddChild(TGLLines.Create(Self));
  SetGraphicAspects;
end;

constructor TArchLine.Create(AOwner: TComponent; ALineType: TArchLineType);
begin
  FLineType:= ALineType;
  Create(AOwner);
end;

function TArchLine.GetLenght: Single;
begin
  Result:= Abs(FStartPoint.DistanceTo(FEndPoint));
end;

function TArchLine.NumOfParts(): integer;
begin
  if (FLineType.LineStyle= lsSolid) then
    begin
      Result:=1;
    end
    else begin
      Result:=FLineType.DashesCount + FLineType.GapsCount;
    end;
end;

procedure TArchLine.ClearNodes;
var
  vNodes:TGLLinesNodes;
  GLPoints:TGLPoints;
  i:integer;
begin
  vNodes:= (Children[1] as TGLLines).Nodes;
  for i:= vNodes.Count -1 downto 0 do
  begin
    vNodes.Delete(i);
  end;
  GLPoints:= Children[0] as TGLPoints;
  for i:= GLPoints.Positions.Count-1 downto 0 do
  begin
    GLPoints.Positions.Delete(i);
  end;
end;

procedure TArchLine.DrawDash(aBegin, aEnd: TVector3f);
var
  vNodes:TGLLinesNodes;
  vGLNode: TGLLinesNode;
begin
  vNodes:= (Children[1] as TGLLines).Nodes;
  vNodes.AddNode(aBegin.X,
                 aBegin.Y,
                 aBegin.z);
  vGLNode:= vNodes.Items[vNodes.Count-1] as TGLLinesNode;
  vGLNode.Color:= FStartColor;
  vNodes.AddNode(aEnd.X,
                 aEnd.Y,
                 aEnd.z);
  vGLNode:= vNodes.Items[vNodes.Count-1] as TGLLinesNode;
  vGLNode.Color:= FStartColor;
  DrawDot(aBegin);
  DrawDot(aEnd);
end;

procedure TArchLine.DrawDot(aBegin: TVector3f);
var
  GLPoints: TGLPoints;
begin
  GLPoints:= Children[0] as TGLPoints;
  with GLPoints do
  begin
    Colors.Add(FStartColor.Red,FStartColor.Green,FStartColor.Blue,FStartColor.Alpha);
    Positions.Add(aBegin.X,aBegin.Y, aBegin.Z);
    StructureChanged;
  end;
end;

procedure TArchLine.DrawGap(aBegin, aEnd: TVector3f);
var
  vNodes:TGLLinesNodes;
  vGLNode: TGLLinesNode;
begin
  vNodes:= (Children[1] as TGLLines).Nodes;
  vNodes.AddNode(aBegin.X,
                 aBegin.Y,
                 aBegin.z);
  vGLNode:= vNodes.Items[vNodes.Count-1] as TGLLinesNode;
  vGLNode.Color.Color:= clrTransparent;
  vNodes.AddNode(aEnd.X,
                 aEnd.Y,
                 aEnd.z);
  vGLNode:= vNodes.Items[vNodes.Count-1] as TGLLinesNode;
  vGLNode.Color.Color:= clrTransparent;
end;

procedure TArchLine.AddGap(var restOfLenght:single; aGapIndex:integer);
var
  startPoint,endPoint: TVector3f;
begin
  //ShowMessage('gap');
  startPoint:= (FStartPoint.
                ProjectedPoint((LineLenght-restOfLenght),LineDirectorVector)).
                Vector3FMake;
  if restOfLenght > FLineType.LineTypeGap[aGapIndex] then
  begin
    endPoint:= (FStartPoint.
                ProjectedPoint(((LineLenght-restOfLenght)+ FLineType.LineTypeGap[aGapIndex]),LineDirectorVector)).
                Vector3FMake;
    DrawGap(startPoint,endPoint);
    restOfLenght:=restOfLenght-FLineType.LineTypeGap[aGapIndex];
  end else
  begin
    endPoint:= FEndPoint.Vector3FMake;
    DrawGap(startPoint,endPoint);
    DrawDot(endPoint);
    restOfLenght:=0;
  end;
end;

procedure TArchLine.AddDashed();
var
  i:integer;
  restOfLenght:Single;
  startPoint,endPoint: TVector3f;
begin
  restOfLenght:=LineLenght;
  //startPoint:= FStartPoint.Vector3FMake;
    while (restOfLenght>0) do
    begin
      for i:= 0 to FLineType.DashesCount-1 do
      begin
        startPoint:=(FStartPoint.
                     ProjectedPoint((LineLenght-restOfLenght),LineDirectorVector)).
                     Vector3FMake;
        if (FLineType.LineTypeDash[i]=0) then
        begin
          //ShowMessage('ponto');
          DrawDot(startPoint);
          AddGap(restOfLenght,i);
        end else
        begin
          startPoint:=(FStartPoint.
                       ProjectedPoint((LineLenght-restOfLenght),LineDirectorVector)).
                       Vector3FMake;
          if (restOfLenght > FLineType.LineTypeDash[i]) then
          begin
            endPoint:= (FStartPoint.
                        ProjectedPoint(((LineLenght-restOfLenght)+FLineType.LineTypeDash[i]),LineDirectorVector)).
                        Vector3FMake;
            //ShowMessage('traço');
            DrawDash(startPoint,endPoint);
            restOfLenght:= restOfLenght - FLineType.LineTypeDash[i];
            AddGap(restOfLenght,i);
          end else
          begin
            endPoint:= FEndPoint.Vector3FMake;
            DrawDash(startPoint,endPoint);
            restOfLenght:=0;
          end;
        end;
      end;
    end;
end;

procedure TArchLine.SetStarPoint(AX, AY, AZ: Single);
begin
  FStartPoint.SetVector(AX,AY,AZ);
  UpdateNode;
end;

procedure TArchLine.SetStartPoint(APoint: TArch3DPoint);
begin
  SetStarPoint(APoint.X, APoint.Y, APoint.Z);
end;

procedure TArchLine.SetEndPoint(AX, AY, AZ: Single);
begin
  FEndPoint.SetVector(AX, AY, AZ);
  UpdateNode;
end;

procedure TArchLine.SetEndPoint(APoint: TArch3DPoint);
begin
  SetEndPoint(APoint.X, APoint.Y, APoint.Z);
end;

procedure TArchLine.SetLineType(ALineType: TArchLineType);
begin
  FLineType:=ALineType;
  UpdateNode;
end;

{ TArchMakeWallLine }

constructor TArchMakeWallLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,ARCH_MAKE_LINE);
  AddChild(TGLLines.Create(Self));
  with Children[0] as TGLLines do
  begin
    AntiAliased:= True;
    LineColor.Color:= clrBlack ;
    LineWidth:=0.5;
    NodesAspect:= lnaInvisible;
    Pickable:= True;
    SplineMode:= lsmLines;
  end;
end;

procedure TArchMakeWallLine.StartingWall(ANodes: TArch3DPointArray);
var
  i:integer;
begin
  with Children[0] as TGLLines do
  begin
    Nodes.Clear;
    for i:=0 to (Length(ANodes)-1) do
    begin
     AddNode(ANodes[i].GLCoordinatesMake(Self));
    end;
  end;
end;

{ TArchReferenceWallLine }

constructor TArchReferenceWallLine.Create(AOwner: TComponent; AStartNode,AEndNode:IArch3DPoint);
begin
  inherited Create(AOwner,ARCH_REFERENCE_LINE);
  AddChild(TGLLines.Create(Self));
  with Children[0] as TGLLines do
  begin
    AntiAliased:= True;
    LineColor.Color:= clrBlue ;
    NodeColor.Color:= clrBlack;
    NodesAspect:= lnaInvisible;
    Pickable:= True;
    SplineMode:= lsmLines;
    AddNode(AStartNode.GLCoordinatesMake(Self));
    AddNode(AEndNode.GLCoordinatesMake(Self));
  end;
  AddChild(TArchVertex.Create(Self,AStartNode.GLCoordinatesMake(Self)));
  AddChild(TArchVertex.Create(Self,AEndNode.GLCoordinatesMake(Self)));
end;

procedure TArchReferenceWallLine.EditWall(AStartNode, AEndNode: IArch3DPoint);
begin
  with Children[0] as TGLLines do
  begin
    Nodes.Items[0].X:=AStartNode.X;
    Nodes.Items[0].Y:=AStartNode.Y;
    Nodes.Items[0].Z:=AStartNode.Z;
    Nodes.Items[1].X:=AEndNode.x;
    Nodes.Items[1].y:=AEndNode.y;
    Nodes.Items[1].z:=AEndNode.z;
    LineColor.Color:= clrBlue;
    LineWidth:= 1.8;
  end;
  with Children[1] as TArchVertex do
  begin
    SetCoordinate(AStartNode.GLCoordinatesMake(Self));
  end;
  with Children[2] as TArchVertex do
  begin
    SetCoordinate(AEndNode.GLCoordinatesMake(Self));
  end;
end;

procedure TArchReferenceWallLine.StartingWall(AStartNode,
  AEndNode: IArch3DPoint);
begin
  with Children[0] as TGLLines do
  begin
    Nodes.Items[0].X:=AStartNode.X;
    Nodes.Items[0].Y:=AStartNode.Y;
    Nodes.Items[0].Z:=AStartNode.Z;
    Nodes.Items[1].X:=AEndNode.x;
    Nodes.Items[1].y:=AEndNode.y;
    Nodes.Items[1].z:=AEndNode.z;
    LineColor.Color:= clrBlack;
    LineWidth:= 1.5;
  end;
  with Children[1] as TArchVertex do
  begin
    SetCoordinate(AStartNode.GLCoordinatesMake(Self));
  end;
  with Children[2] as TArchVertex do
  begin
    SetCoordinate(AEndNode.GLCoordinatesMake(Self));
  end;
end;

end.

