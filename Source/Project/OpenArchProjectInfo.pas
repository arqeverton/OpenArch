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
2020 08 20 - ET - Unit implementation Start's.
}

unit OpenArchProjectInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //OpenArch;
  OpenArchSysParameters;

type

{ ------------------------------------------------------------------------
                         TArchSiteInfo
------------------------------------------------------------------------- }

  TArchSiteInfo= class
    private
      FSiteName,
      FSiteDescription,
      FSiteID,
      FSiteFullAddress: String;
      FSiteArea: Double;
      FSiteCustomParameters: TArchParamList;
      procedure SetSiteArea(AValue: Double);
      procedure SetSiteCustomParameters(AValue: TArchParamList);
      procedure SetSiteDescription(AValue: string);
      procedure SetSiteFullAddress(AValue: string);
      procedure SetSiteID(AValue: string);
      procedure SetSiteName(AValue: string);
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadData(AParameters: TArchParamList);
      procedure AddCustomParameter(AValue: TArchParameter);
      procedure ClearCustomParameters;
      property SiteName:string read FSiteName write SetSiteName;
      property SiteDescription:string read FSiteDescription write SetSiteDescription;
      property SiteID:string read FSiteID write SetSiteID;
      property SiteFullAddress:string read FSiteFullAddress write SetSiteFullAddress;
      property SiteArea:Double read FSiteArea write SetSiteArea;
      property SiteCustomParameters: TArchParamList read FSiteCustomParameters write SetSiteCustomParameters;
  end;

{ ------------------------------------------------------------------------
                         TArchBuildingInfo
------------------------------------------------------------------------- }

  TArchBuildingInfo= class
    private
      FBuildingName,
      FBuildingDescription,
      FBuildingID:string;
      FBuildingCustomParameters: TArchParamList;
      procedure SetBuildingCustomParameters(AValue: TArchParamList);
      procedure SetBuildingDescription(AValue: string);
      procedure SetBuildingID(AValue: string);
      procedure SetBuildingName(AValue: string);
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadData(AParameters: TArchParamList);
      procedure AddCustomParameter(AValue: TArchParameter);
      procedure ClearCustomParameters;
      property BuildingName:string read FBuildingName write SetBuildingName;
      property BuildingDescription:string read FBuildingDescription write SetBuildingDescription;
      property BuildingID:string read FBuildingID write SetBuildingID;
      property BuildingCustomParameters:TArchParamList read FBuildingCustomParameters write SetBuildingCustomParameters;
  end;

{ ------------------------------------------------------------------------
                         TArchDesignerInfo
------------------------------------------------------------------------- }

  TArchDesignerInfo = class
    private
      FDesignerName,
      FDesignerIDRegister:string;
      FDesignerCustomParameters: TArchParamList;
      procedure SetDesignerCustomParameters(AValue: TArchParamList);
      procedure SetDesignerIDRegister(AValue: string);
      procedure SetDesignerName(AValue: String);
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadData(AParameters: TArchParamList);
      procedure AddCustomParameter(AValue: TArchParameter);
      procedure ClearCustomParameters;
      property DesignerName:String read FDesignerName write SetDesignerName;
      property DesignerIDRegister:string read FDesignerIDRegister write SetDesignerIDRegister;
      property DesignerCustomParameters:TArchParamList read FDesignerCustomParameters write SetDesignerCustomParameters;
  end;

{ ------------------------------------------------------------------------
                         TArchCustomerInfo
------------------------------------------------------------------------- }

  TArchCustomerInfo = class
    private
      FCustomerName,
      FCustomerID,
      FCustomerEmail:String;
      FCustomerCustomParameters: TArchParamList;
      procedure SetCustomerCustomParameters(AValue: TArchParamList);
      procedure SetCustomerEmail(AValue: string);
      procedure SetCustomerID(AValue: string);
      procedure SetCustomerName(AValue: string);
      procedure SetFCustomerName(AValue: string);
    public
      procedure LoadData(AParameters: TArchParamList);
      constructor Create;
      destructor Destroy; override;
      procedure AddCustomParameter(AValue: TArchParameter);
      procedure ClearCustomParameters;
      property CustomerName:string read FCustomerName write SetCustomerName;
      property CustomerID:string read FCustomerID write SetCustomerID;
      property CustomerEmail:string read FCustomerEmail write SetCustomerEmail;
      property CustomerCustomParameters:TArchParamList read FCustomerCustomParameters write SetCustomerCustomParameters;
  end;

{ ------------------------------------------------------------------------
                         TArchProjectInfo
------------------------------------------------------------------------- }

  TArchProjectInfo = class
    private
      FProjectName,
      FProjectDescription,
      FProjectID,
      FProjectPhase,
      FKeyWords,
      FNotes,
      FTemplateAuthor: String;
      FCustomParameters: TArchParamList;
      FSiteInfo: TArchSiteInfo;
      FBuildingInfo: TArchBuildingInfo;
      FDesignerinfo: TArchDesignerInfo;
      FCustomerInfo: TArchCustomerInfo;
      procedure SetBuildingInfo(AValue: TArchBuildingInfo);
      procedure SetCustomerInfo(AValue: TArchCustomerInfo);
      procedure SetDesignerInfo(AValue: TArchDesignerInfo);
      procedure SetFCustomParameters(AValue: TArchParamList);
      procedure SetKeyWords(AValue: string);
      procedure SetNotes(AValue: string);
      procedure SetProjectDescription(AValue: string);
      procedure SetProjectID(AValue: string);
      procedure SetProjectName(AValue: String);
      procedure SetProjectPhase(AValue: string);
      procedure SetSiteinfo(AValue: TArchSiteInfo);
      procedure SetTemplateAuthor(AValue: string);
    public
      constructor Create;
      destructor Destroy;
      procedure AddCustomParameter(AParameter: TArchParameter);
      procedure DelCustomParameter(AIndex:integer);
      procedure LoadData(AParameters: TArchParamList);
      procedure ClearCustomParameters;
      property ProjectName:String read FProjectName write SetProjectName;
      property ProjectDescription:string read FProjectDescription write SetProjectDescription;
      property ProjectID:string read FProjectID write SetProjectID;
      property ProjectPhase:string read FProjectPhase write SetProjectPhase;
      property KeyWords:string read FKeyWords write SetKeyWords;
      property Notes:string read FNotes write SetNotes;
      property Siteinfo:TArchSiteInfo read FSiteinfo write SetSiteinfo;
      property BuildingInfo:TArchBuildingInfo read FBuildingInfo write SetBuildingInfo;
      property DesignerInfo:TArchDesignerInfo read FDesignerInfo write SetDesignerInfo;
      property CustomerInfo:TArchCustomerInfo read FCustomerInfo write SetCustomerInfo;
      property TemplateAuthor:string read FTemplateAuthor write SetTemplateAuthor;
      property CustomParameters:TArchParamList read FCustomParameters write SetFCustomParameters;
  end;

{======================================================================================}
implementation
{======================================================================================}
uses
  Dialogs;
{ ------------------------------------------------------------------------
                         TArchSiteInfo
------------------------------------------------------------------------- }
procedure TArchSiteInfo.SetSiteName(AValue: string);
begin
  if FSiteName=AValue then Exit;
  FSiteName:=AValue;
end;

procedure TArchSiteInfo.SetSiteDescription(AValue: string);
begin
  if FSiteDescription=AValue then Exit;
  FSiteDescription:=AValue;
end;

procedure TArchSiteInfo.SetSiteArea(AValue: Double);
begin
  if FSiteArea=AValue then Exit;
  FSiteArea:=AValue;
end;

procedure TArchSiteInfo.SetSiteCustomParameters(AValue: TArchParamList);
begin
  if FSiteCustomParameters=AValue then Exit;
  FSiteCustomParameters:=AValue;
end;

procedure TArchSiteInfo.SetSiteFullAddress(AValue: string);
begin
  if FSiteFullAddress=AValue then Exit;
  FSiteFullAddress:=AValue;
end;

procedure TArchSiteInfo.SetSiteID(AValue: string);
begin
  if FSiteID=AValue then Exit;
  FSiteID:=AValue;
end;

constructor TArchSiteInfo.Create;
begin

end;

destructor TArchSiteInfo.Destroy;
var
  i:integer;
begin
  inherited Destroy;
  for i:=Length(FSiteCustomParameters) downto 0 do
  begin
    FSiteCustomParameters[i].Free;
  end;
  SetLength(FSiteCustomParameters,0);
end;

procedure TArchSiteInfo.LoadData(AParameters: TArchParamList);
var
  i,vCount:integer;
  vCustomParameter:Boolean;
begin
  DecimalSeparator:='.';
  vCustomParameter:=false;
  vCount:=0;
  for i:= 0 to Length(AParameters)-1 do
  begin
    if vCustomParameter then
    begin
      if not (AParameters[i].Value= ARCH_ENDENTITY) then
      begin
        vCount:=vCount+1;
        SetLength(FSiteCustomParameters,vCount);
        FSiteCustomParameters[vCount-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
      end;
    end;
    case AParameters[i].ParamName of
      ARCH_SITE_NAME          : SiteName:=        AParameters[i].Value;
      ARCH_SITE_DESCRIPITION  : SiteDescription:= AParameters[i].Value;
      ARCH_SITEID             : SiteID:=          AParameters[i].Value;
      ARCH_SITE_FULLADDRESS   : SiteFullAddress:= AParameters[i].Value;
      ARCH_SITE_AREA          : SiteArea:=        StrToFloat(AParameters[i].Value);
      ARCH_STARTDATA :
        begin
          case AParameters[i].Value of
            ARCH_SITE_CUSTOM_PARAMETERS : vCustomParameter:=true;
            ARCH_ENDENTITY : vCustomParameter:=false;
          end;
        end;
    end;
  end;
  DecimalSeparator:=',';
end;

procedure TArchSiteInfo.AddCustomParameter(AValue: TArchParameter);
begin
  SetLength(FSiteCustomParameters,Length(FSiteCustomParameters)+1);
  FSiteCustomParameters[Length(FSiteCustomParameters)-1]:= AValue;
end;

procedure TArchSiteInfo.ClearCustomParameters;
begin
  SetLength(FSiteCustomParameters,0);
end;

{ ------------------------------------------------------------------------
                         TArchBuildingInfo
------------------------------------------------------------------------- }
procedure TArchBuildingInfo.SetBuildingName(AValue: string);
begin
  if FBuildingName=AValue then Exit;
  FBuildingName:=AValue;
end;

procedure TArchBuildingInfo.SetBuildingDescription(AValue: string);
begin
  if FBuildingDescription=AValue then Exit;
  FBuildingDescription:=AValue;
end;

procedure TArchBuildingInfo.SetBuildingCustomParameters(AValue: TArchParamList);
begin
  if FBuildingCustomParameters=AValue then Exit;
  FBuildingCustomParameters:=AValue;
end;

procedure TArchBuildingInfo.SetBuildingID(AValue: string);
begin
  if FBuildingID=AValue then Exit;
  FBuildingID:=AValue;
end;

constructor TArchBuildingInfo.Create;
begin

end;

destructor TArchBuildingInfo.Destroy;
var
  i:integer;
begin
  inherited Destroy;
  for i:=Length(FBuildingCustomParameters) downto 0 do
  begin
    FBuildingCustomParameters[i].Free;
  end;
  SetLength(FBuildingCustomParameters,0);
end;

procedure TArchBuildingInfo.LoadData(AParameters: TArchParamList);
var
  i,vCount:integer;
  vCustomParameter:Boolean;
begin
  vCustomParameter:=false;
  vCount:=0;
  for i:= 0 to Length(AParameters)-1 do
  begin
    if vCustomParameter then
    begin
      if not (AParameters[i].Value= ARCH_ENDENTITY) then
      begin
        vCount:=vCount+1;
        SetLength(FBuildingCustomParameters,vCount);
        FBuildingCustomParameters[vCount-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
      end;
    end;
    case AParameters[i].ParamName of
      ARCH_BUILDING_NAME          : BuildingName:=        AParameters[i].Value;
      ARCH_BUILDING_DESCRIPITION  : BuildingDescription:= AParameters[i].Value;
      ARCH_BUILDINGID             : BuildingID:=          AParameters[i].Value;
      ARCH_STARTDATA :
        begin
          case AParameters[i].Value of
            ARCH_BUILDING_CUSTOM_PARAMETERS : vCustomParameter:=true;
            ARCH_ENDENTITY : vCustomParameter:=false;
          end;
        end;
    end;
  end;
end;

procedure TArchBuildingInfo.AddCustomParameter(AValue: TArchParameter);
begin
  SetLength(FBuildingCustomParameters,Length(FBuildingCustomParameters)+1);
  FBuildingCustomParameters[Length(FBuildingCustomParameters)-1]:= AValue;
end;

procedure TArchBuildingInfo.ClearCustomParameters;
begin
  SetLength(FBuildingCustomParameters,0);
end;

{ ------------------------------------------------------------------------
                         TArchDesignerInfo
------------------------------------------------------------------------- }
procedure TArchDesignerInfo.SetDesignerName(AValue: String);
begin
  if FDesignerName=AValue then Exit;
  FDesignerName:=AValue;
end;

procedure TArchDesignerInfo.SetDesignerIDRegister(AValue: string);
begin
  if FDesignerIDRegister=AValue then Exit;
  FDesignerIDRegister:=AValue;
end;

procedure TArchDesignerInfo.SetDesignerCustomParameters(AValue: TArchParamList);
begin
  if FDesignerCustomParameters=AValue then Exit;
  FDesignerCustomParameters:=AValue;
end;

constructor TArchDesignerInfo.Create;
begin

end;

destructor TArchDesignerInfo.Destroy;
var
  i:integer;
begin
  inherited Destroy;
  for i:=Length(FDesignerCustomParameters) downto 0 do
  begin
    FDesignerCustomParameters[i].Free;
  end;
  SetLength(FDesignerCustomParameters,0);
end;

procedure TArchDesignerInfo.LoadData(AParameters: TArchParamList);
var
  i,vCount:integer;
  vCustomParameter:Boolean;
begin
  vCustomParameter:=false;
  vCount:=0;
  for i:= 0 to Length(AParameters)-1 do
  begin
    if vCustomParameter then
    begin
      if not (AParameters[i].Value= ARCH_ENDENTITY) then
      begin
        vCount:=vCount+1;
        SetLength(FDesignerCustomParameters,vCount);
        FDesignerCustomParameters[vCount-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
      end;
    end;
    case AParameters[i].ParamName of
      ARCH_DESIGNER_NAME          : DesignerName:= AParameters[i].Value;
      ARCH_DESIGNER_IDREGISTER    : DesignerIDRegister:=   AParameters[i].Value;
      ARCH_STARTDATA :
        begin
          case AParameters[i].Value of
            ARCH_DESIGNER_CUSTOM_PARAMETERS : vCustomParameter:=true;
            ARCH_ENDENTITY : vCustomParameter:=false;
          end;
        end;
    end;
  end;
end;

procedure TArchDesignerInfo.AddCustomParameter(AValue: TArchParameter);
begin
  SetLength(FDesignerCustomParameters,Length(FDesignerCustomParameters)+1);
  FDesignerCustomParameters[Length(FDesignerCustomParameters)-1]:= AValue;
end;

procedure TArchDesignerInfo.ClearCustomParameters;
begin
  SetLength(FDesignerCustomParameters,0);
end;

{ ------------------------------------------------------------------------
                         TArchCustomerInfo
------------------------------------------------------------------------- }
procedure TArchCustomerInfo.SetFCustomerName(AValue: string);
begin
  if FCustomerName=AValue then Exit;
  FCustomerName:=AValue;
end;

procedure TArchCustomerInfo.LoadData(AParameters: TArchParamList);
var
  i,vCount:integer;
  vCustomParameter:Boolean;
begin
  vCustomParameter:=false;
  vCount:=0;
  for i:= 0 to Length(AParameters)-1 do
  begin
    if vCustomParameter then
    begin
      if not (AParameters[i].Value= ARCH_ENDENTITY) then
      begin
        vCount:=vCount+1;
        SetLength(FCustomerCustomParameters,vCount);
        FCustomerCustomParameters[vCount-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
      end;
    end;
    case AParameters[i].ParamName of
      ARCH_CUSTOMER_NAME : CustomerName:= AParameters[i].Value;
      ARCH_CUSTOMERID    : CustomerID:=   AParameters[i].Value;
      ARCH_CUSTOMER_EMAIL: CustomerEmail:=AParameters[i].Value;
      ARCH_STARTDATA :
        begin
          case AParameters[i].Value of
            ARCH_CUSTOMER_CUSTOM_PARAMETERS : vCustomParameter:=true;
            ARCH_ENDENTITY : vCustomParameter:=false;
          end;
        end;
    end;
  end;
end;

procedure TArchCustomerInfo.SetCustomerID(AValue: string);
begin
  if FCustomerID=AValue then Exit;
  FCustomerID:=AValue;
end;

procedure TArchCustomerInfo.SetCustomerName(AValue: string);
begin
  if FCustomerName=AValue then Exit;
  FCustomerName:=AValue;
end;

procedure TArchCustomerInfo.SetCustomerEmail(AValue: string);
begin
  if FCustomerEmail=AValue then Exit;
  FCustomerEmail:=AValue;
end;

procedure TArchCustomerInfo.SetCustomerCustomParameters(AValue: TArchParamList);
begin
  if FCustomerCustomParameters=AValue then Exit;
  FCustomerCustomParameters:=AValue;
end;

constructor TArchCustomerInfo.Create;
begin

end;

destructor TArchCustomerInfo.Destroy;
var
  i:integer;
begin
  inherited Destroy;
  for i:=Length(FCustomerCustomParameters) downto 0 do
  begin
    FCustomerCustomParameters[i].Free;
  end;
  SetLength(FCustomerCustomParameters,0);
end;

procedure TArchCustomerInfo.AddCustomParameter(AValue: TArchParameter);
begin
  SetLength(FCustomerCustomParameters,Length(FCustomerCustomParameters)+1);
  FCustomerCustomParameters[Length(FCustomerCustomParameters)-1]:= AValue;
end;

procedure TArchCustomerInfo.ClearCustomParameters;
begin
  SetLength(FCustomerCustomParameters,0);
end;



{ ------------------------------------------------------------------------
                         TArchProjectInfo
------------------------------------------------------------------------- }
procedure TArchProjectInfo.SetProjectName(AValue: String);
begin
  if FProjectName=AValue then Exit;
  FProjectName:=AValue;
end;

procedure TArchProjectInfo.SetProjectPhase(AValue: string);
begin
  if FProjectPhase=AValue then Exit;
  FProjectPhase:=AValue;
end;

procedure TArchProjectInfo.SetSiteinfo(AValue: TArchSiteInfo);
begin
  if FSiteinfo=AValue then Exit;
  FSiteinfo:=AValue;
end;

procedure TArchProjectInfo.SetTemplateAuthor(AValue: string);
begin
  if FTemplateAuthor=AValue then Exit;
  FTemplateAuthor:=AValue;
end;

procedure TArchProjectInfo.SetProjectDescription(AValue: string);
begin
  if FProjectDescription=AValue then Exit;
  FProjectDescription:=AValue;
end;

procedure TArchProjectInfo.SetKeyWords(AValue: string);
begin
  if FKeyWords=AValue then Exit;
  FKeyWords:=AValue;
end;

procedure TArchProjectInfo.SetBuildingInfo(AValue: TArchBuildingInfo);
begin
  if FBuildingInfo=AValue then Exit;
  FBuildingInfo:=AValue;
end;

procedure TArchProjectInfo.SetCustomerInfo(AValue: TArchCustomerInfo);
begin
  if FCustomerInfo=AValue then Exit;
  FCustomerInfo:=AValue;
end;

procedure TArchProjectInfo.SetDesignerInfo(AValue: TArchDesignerInfo);
begin
  if FDesignerInfo=AValue then Exit;
  FDesignerInfo:=AValue;
end;

procedure TArchProjectInfo.SetFCustomParameters(AValue: TArchParamList);
begin
  if FCustomParameters=AValue then Exit;
  FCustomParameters:=AValue;
end;

procedure TArchProjectInfo.SetNotes(AValue: string);
begin
  if FNotes=AValue then Exit;
  FNotes:=AValue;
end;

procedure TArchProjectInfo.SetProjectID(AValue: string);
begin
  if FProjectID=AValue then Exit;
  FProjectID:=AValue;
end;

constructor TArchProjectInfo.Create;
begin
  FSiteInfo:= TArchSiteInfo.Create;
  FBuildingInfo:= TArchBuildingInfo.Create;
  FDesignerinfo:= TArchDesignerInfo.Create;
  FCustomerInfo:= TArchCustomerInfo.Create;
end;

destructor TArchProjectInfo.Destroy;
var
  i:integer;
begin
  FSiteInfo.Free;
  FBuildingInfo.Free;
  FDesignerinfo.Free;
  FCustomerInfo.Free;
  for i:=Length(FCustomParameters) downto 0 do
  begin
    FCustomParameters[i].Free;
  end;
  SetLength(FCustomParameters,0);
end;

procedure TArchProjectInfo.AddCustomParameter(AParameter: TArchParameter);
begin
  SetLength(FCustomParameters,Length(FCustomParameters)+1);
  FCustomParameters[Length(FCustomParameters)-1]:= AParameter;
end;

procedure TArchProjectInfo.DelCustomParameter(AIndex: integer);
var
  i:integer;
  vParamList:TArchParamList;
begin
  for i:= 0 to Length(CustomParameters)-1 do
  begin
    if not (i = AIndex) then
    begin
      SetLength(vParamList,Length(vParamList)+1);
      vParamList[Length(vParamList)-1]:= CustomParameters[i];
    end;
  end;
  SetLength(FCustomParameters,0);
  CustomParameters:= vParamList;
end;

procedure TArchProjectInfo.LoadData(AParameters: TArchParamList);
var
  i,vCount:integer;
  vCustomParameter, vSInfo, vBInfo,
  vDInfo, vCInfo:Boolean;
  vSiteParams, vBuildingParams, vDesignerParams,
  vCustomerParams: TArchParamList;
begin
  vCustomParameter:=false;
  vSInfo:=false;
  vBInfo:=false;
  vDInfo:=false;
  vCInfo:=false;
  vCount:=0;
  for i:= 0 to Length(AParameters)-1 do
  begin
    if vCustomParameter then
    begin
      if not (AParameters[i].Value= ARCH_ENDENTITY) then
      begin
        vCount:=vCount+1;
        SetLength(FCustomParameters,vCount);
        FCustomParameters[vCount-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
      end;
    end;
    case AParameters[i].ParamName of
      ARCH_PROJECT_NAME            : ProjectName        := AParameters[i].Value;
      ARCH_PROJECT_DESCRIPTION     : ProjectDescription := AParameters[i].Value;
      ARCH_PROJECTID               : ProjectID          := AParameters[i].Value;
      ARCH_PROJECT_PHASE           : ProjectPhase       := AParameters[i].Value;
      ARCH_PROJECT_KEYWORDS        : KeyWords           := AParameters[i].Value;
      ARCH_PROJECT_NOTES           : Notes              := AParameters[i].Value;
      ARCH_PROJECT_TEMPLATE_AUTHOR : TemplateAuthor     := AParameters[i].Value;
      ARCH_STARTDATA :
        begin
          case AParameters[i].Value of
            ARCH_PROJECT_CUSTOMPARAMETERS : vCustomParameter:=true;
            ARCH_SITE_INFO :
              begin
                vSInfo:=true;
                vBInfo:=false;
                vDInfo:=false;
                vCInfo:=false;
              end;
            ARCH_BUILDING_INFO :
              begin
                vSInfo:=false;
                vBInfo:=true;
                vDInfo:=false;
                vCInfo:=false;
              end;
            ARCH_DESIGNER_INFO :
              begin
                vSInfo:=false;
                vBInfo:=false;
                vDInfo:=true;
                vCInfo:=false;
              end;
            ARCH_CUSTOMER_INFO :
              begin
                vSInfo:=false;
                vBInfo:=false;
                vDInfo:=false;
                vCInfo:=true;
              end;
            ARCH_ENDENTITY : vCustomParameter:=false;
          end;
        end;
    end;
    if vSInfo then
    begin
      SetLength(vSiteParams,(Length(vSiteParams)+1));
      vSiteParams[(Length(vSiteParams))-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
    end;
    if vBInfo then
    begin
      SetLength(vBuildingParams,(Length(vBuildingParams)+1));
      vBuildingParams[(Length(vBuildingParams))-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
    end;
    if vDInfo then
    begin
      SetLength(vDesignerParams,(Length(vDesignerParams)+1));
      vDesignerParams[(Length(vDesignerParams))-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
    end;
    if vCInfo then
    begin
      SetLength(vCustomerParams,(Length(vCustomerParams)+1));
      vCustomerParams[(Length(vCustomerParams))-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
    end;
  end; // Finish For
  Siteinfo.LoadData(vSiteParams);
  BuildingInfo.LoadData(vBuildingParams);
  DesignerInfo.LoadData(vDesignerParams);
  CustomerInfo.LoadData(vCustomerParams);
end;

procedure TArchProjectInfo.ClearCustomParameters;
begin
  SetLength(FCustomParameters,0);
end;

end.

