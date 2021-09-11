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
 2016 09 20 - ET - Unit implementation Start's.
}

unit OpenArchTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DateUtils, Dialogs,
  //OpenArch:

  //GLScene:
  GLScene, GLVectorGeometry, GLObjects, GLColor, GLCoordinates, OpenGLTokens;

type

  { TArchMouseEvent }
  TArchMouseEvent = (meMouseDown, meMouseUp, meMouseMove,
                     meMoseWheel, meMouseWheelUp, meMouseWheelDown,
                     meNoMouse);

  { TArchProjectState }
  TArchProjectState = (psStandBy, psSelect, psNewElement, psStartingElement,
                       psEditElement, psFinishingElement);
  { TArchEnviromentType }
  TArchEnviromentType = (etFloorPlan, et3d);

  { TArchElementType }
  TArchElementType= (etStandBy,etWall);

  { TArchWallState }
  TArchWallState = (wsStandBy, wsNewWall, wsEditWall);

  { TArchWallRefLinePosition }
  TArchWallRefLinePosition = (rlOutiside, rlInside, rlCenter, rlOutsideCore,
                              rlInsideCore, rlCenterCore);

  { TArchVectorArray }
  TArchVectorArrays = array of array of TVector;

  { TArchBoolArray }
  TArchBoolArray = array of Boolean;

  { TArchEntity }
  TArchEntity = class(TGLBaseSceneObject)
    private
      FEntityName: string;
      FID: string;
      procedure SetID(AValue: string);
      procedure SetEntityName(AValue: string);
      function GetUniqueID(const AEntityName: string):String;
    protected
      FEnviromentType: TArchEnviromentType;
    public
      procedure SetEnviroment(AEnviroment:TArchEnviromentType);
      constructor Create(AOwner: TComponent; AName:String); overload;
      property EntityName:string read FEntityName write SetEntityName;
      property ID:string read FID write SetID;
  end;

  { TArchVertex }

  TArchVertex = class(TArchEntity)
    private
    public
      constructor Create(AOwner: TComponent; APosition:TGLCoordinates); overload;
      procedure SetCoordinate(APosition:TGLCoordinates);
  end;

  { TArchConextinfo }
  TArchContextinfo = Record
    MouseEvent: TArchMouseEvent;
    MouseButton: TMouseButton;
    MouseShiftState: TShiftState;
    Coordinates2D: TVector;
  end;

  { TArchParam }
  TArchParam = record
    ParamName: String;
    case ParamType:integer of
      0:   (ParamBolValue: Boolean);
      1:   (ParamIntValue: Integer);
      2:   (ParamFltValue: TGLfloat);
      3:   (ParamStrValue: String[255]);
      4:   (ParamMEValue:  TArchMouseEvent);
      5:   (ParamPSValue:  TArchProjectState);
      6:   (ParamETValue:  TArchElementType);
      7:   (ParamCIValue:  TArchContextinfo);
      8:   (ParamEVTValue: TArchEnviromentType);
      9:  (ParamSngValue: Single);
  end;

  { TArchMessage }
  TArchMessage = Array of TArchParam;

implementation

uses
  OpenArchSysParameters;
{ TArchVertex }

constructor TArchVertex.Create(AOwner: TComponent; APosition: TGLCoordinates);
begin
  inherited Create(AOwner,ARCH_VERTEX);
  AddChild(TGLPoints.Create(Self));
  Position.SetPoint(0,0,0);
  with Children[0] as TGLPoints do
  begin
    Size:=5;
    Colors.Add(clrBlack);
    Style:=psSmooth;
    Position:= APosition;
  end;
end;

procedure TArchVertex.SetCoordinate(APosition: TGLCoordinates);
begin
  with Children[0] as TGLPoints do
  begin
    Position:= APosition;
  end;
end;

{ TArchEntity }

procedure TArchEntity.SetID(AValue: string);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TArchEntity.SetEntityName(AValue: string);
begin
  if FEntityName=AValue then Exit;
  FEntityName:=AValue;
end;

{This function serves to get a unique ID to the any Entity.}
function TArchEntity.GetUniqueID(const AEntityName: string): String;
var
   vMilliSeconds: Int64;
begin
  if AEntityName.IsEmpty then Result:= ARCH_ENTITY
    else Result:= AEntityName;
  Sleep(1);
  vMilliSeconds:=MilliSecondsBetween(DateDelta,Now);
  Result:= Result + vMilliSeconds.ToString();
end;

procedure TArchEntity.SetEnviroment(AEnviroment: TArchEnviromentType);
begin
  FEnviromentType:=AEnviroment;
end;

constructor TArchEntity.Create(AOwner: TComponent; AName: String);
begin
  inherited Create(AOwner);
  EntityName:=AName;
  FID:=GetUniqueID(AName);
end;

end.

