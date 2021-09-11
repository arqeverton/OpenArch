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
 2021 04 16 - ET - Unit implementation Start's.
}
unit OpenArchProjectInfoForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Grids, PairSplitter, Buttons, ValEdit, TplListBoxUnit,
  TplSideBarUnit,
  // OpenArch:
  OpenArchProjectInfo;

type

  { TProjectInfoForm }

  TProjectInfoForm = class(TForm)
    BitBtnBuildingInfoAdd: TBitBtn;
    BitBtnBuildingInfoDel: TBitBtn;
    BitBtnCustomerInfoAdd: TBitBtn;
    BitBtnCustomerInfoDel: TBitBtn;
    BitBtnDesignerInfoAdd: TBitBtn;
    BitBtnDesignerInfoDel: TBitBtn;
    BitBtnProjectInfoAdd: TBitBtn;
    BitBtnProjectInfoDel: TBitBtn;
    BitBtnSiteInfoAdd: TBitBtn;
    BitBtnSiteInfoDel: TBitBtn;
    btnOK: TButton;
    btnCancel: TButton;
    labNotes: TLabel;
    labInfoAdd: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbCustomerID: TLabeledEdit;
    lbCustomerName: TLabeledEdit;
    lbCustomerEmail: TLabeledEdit;
    lbDesignerID: TLabeledEdit;
    lbDesignerName: TLabeledEdit;
    lbBuildingID: TLabeledEdit;
    lbBuildingName: TLabeledEdit;
    lbSiteArea: TLabeledEdit;
    lbSiteID: TLabeledEdit;
    lbSiteName: TLabeledEdit;
    lbDescripition: TLabeledEdit;
    lbKeyWords: TLabeledEdit;
    lbProjectID: TLabeledEdit;
    lbProjectPhase: TLabeledEdit;
    lbProjectName: TLabeledEdit;
    lbTemplateAuthor: TLabeledEdit;
    memNotes: TMemo;
    memBuildingDescripition: TMemo;
    memSiteFullAdress: TMemo;
    memSiteDescripition: TMemo;
    pnBuildingInfo: TPanel;
    pnCustomerInfo: TPanel;
    pnDesignerInfo: TPanel;
    pnProjectInfo: TPanel;
    pnSiteInfo: TPanel;
    TabInfoControl: TPageControl;
    pnBotton: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    TabProjectInfo: TTabSheet;
    TabSiteInfo: TTabSheet;
    TabBuildingInfo: TTabSheet;
    TabDesignerInfo: TTabSheet;
    TabCustomerInfo: TTabSheet;
    vleBuildingInfo: TValueListEditor;
    vleCustomerInfo: TValueListEditor;
    vleDesignerInfo: TValueListEditor;
    vleProjectInfo: TValueListEditor;
    vleSiteInfo: TValueListEditor;
    constructor Create(TheOwner: TComponent; AProjectInfo:TArchProjectInfo); overload;
    procedure TranslateCaptions;

    //ProjectInfo:
    procedure LoadProjectInfo(AProjectInfo:TArchProjectInfo);
    procedure BitBtnProjectInfoAddClick(Sender: TObject);
    procedure BitBtnProjectInfoDelClick(Sender: TObject);
    procedure vleProjectInfoSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure SetProjectInfo;

    //SiteInfo:
    procedure LoadSiteInfo(ASInfo: TArchSiteInfo);
    procedure vleSiteInfoSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure BitBtnSiteInfoAddClick(Sender: TObject);
    procedure BitBtnSiteInfoDelClick(Sender: TObject);
    procedure SetSiteInfo;
    procedure lbSiteAreaExit(Sender: TObject);

    //BuildingInfo:
    procedure LoadBuildingInfo(ABInfo: TArchBuildingInfo);
    procedure BitBtnBuildingInfoAddClick(Sender: TObject);
    procedure vleBuildingInfoSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure BitBtnBuildingInfoDelClick(Sender: TObject);
    procedure SetBuildingInfo;

    //DesignerInfo:
    procedure LoadDesignerInfo(ADInfo: TArchDesignerInfo);
    procedure BitBtnDesignerInfoAddClick(Sender: TObject);
    procedure BitBtnDesignerInfoDelClick(Sender: TObject);
    procedure vleDesignerInfoSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure SetDesignerInfo;

    //CustomerInfo:
    procedure LoadCustomerInfo(ACInfo:TArchCustomerInfo);
    procedure BitBtnCustomerInfoAddClick(Sender: TObject);
    procedure vleCustomerInfoSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure BitBtnCustomerInfoDelClick(Sender: TObject);
    procedure SetCustomerInfo;

    // Btn Ok:
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);


  private

  public

  end;

var
  ProjectInfoForm: TProjectInfoForm;
  ProjectInfo: TArchProjectInfo;
  vPRow,
  vSRow,
  vBRow,
  vDRow,
  vCRow: integer;

implementation
uses
  OpenArchTranslate, OpenArchSysParameters;

{$R *.frm}

{ TProjectInfoForm }

constructor TProjectInfoForm.Create(TheOwner: TComponent;
  AProjectInfo: TArchProjectInfo);
begin
  inherited Create(TheOwner);
  ProjectInfo:= AProjectInfo;
  LoadProjectInfo(ProjectInfo);
  //TranslateCaptions; // To Implement after development of the
  // translation System
  vPRow:=0;
  vSRow:=0;
  vBRow:=0;
  vDRow:=0;
  vCRow:=0;
end;

//TO DO:
{Study and Develop Translation System. Checking if doing this translation
at runtime will be more efficient than compiling translated.}
procedure TProjectInfoForm.TranslateCaptions;
begin
  //Tabs:
  TabProjectInfo.Caption        := C_PROJECT_INFO_CAPTION[ActiveLanguage];
  TabSiteInfo.Caption           := C_SITE_INFO_CAPTION[ActiveLanguage];
  TabBuildingInfo.Caption       := C_BUILDING_INFO_CAPTION[ActiveLanguage];
  TabDesignerInfo.Caption       := C_DESIGNER_INFO_CAPTION[ActiveLanguage];
  TabCustomerInfo.Caption       := C_CUSTOMER_INFO_CAPTION[ActiveLanguage];
  //Project Info Captions:
  lbProjectID.EditLabel.Caption      := C_PROJECT_ID[ActiveLanguage];
  lbProjectName.EditLabel.Caption    := C_PROJECT_NAME[ActiveLanguage];
  lbProjectPhase.EditLabel.Caption   := C_PROJECT_PHASE[ActiveLanguage];
  lbTemplateAuthor.EditLabel.Caption := C_TEMPLATE_AUTHOR[ActiveLanguage];
  lbKeyWords.EditLabel.Caption       := C_KEYWORDS[ActiveLanguage];
  lbDescripition.EditLabel.Caption   := C_PROJECT_DESCRIPITION[ActiveLanguage];
  labNotes.Caption                   := C_NOTES[ActiveLanguage];
  labInfoAdd.Caption                 := C_INFO_ADD[ActiveLanguage];
  BitBtnProjectInfoAdd.Caption       := C_ADD[ActiveLanguage];
  BitBtnProjectInfoDel.Caption       := C_REMOVE[ActiveLanguage];
end;

{-----------------------------------------------------------------------------
                             Project Info
-----------------------------------------------------------------------------}
procedure TProjectInfoForm.LoadProjectInfo(AProjectInfo: TArchProjectInfo);
var
  i:integer;
begin
  with AProjectInfo do
  begin
    lbProjectID.Text      := ProjectID;
    lbProjectName.Text    := ProjectName;
    lbProjectPhase.Text   := ProjectPhase;
    lbDescripition.Text   := ProjectDescription;
    lbKeyWords.Text       := KeyWords;
    lbTemplateAuthor.Text := TemplateAuthor;
    memNotes.Lines.Add(Notes);
    with vleProjectInfo do
    begin
      RowCount:=0;
      for i:= 0 to Length(CustomParameters)-1 do
      begin
        InsertRow(CustomParameters[i].ParamName,CustomParameters[i].Value, true );
      end;
    end;
    LoadSiteInfo(Siteinfo);
    LoadBuildingInfo(BuildingInfo);
    LoadDesignerInfo(DesignerInfo);
    LoadCustomerInfo(CustomerInfo);
  end;
end;

procedure TProjectInfoForm.BitBtnProjectInfoAddClick(Sender: TObject);
begin
  vleProjectInfo.InsertRow('','',true);
end;

procedure TProjectInfoForm.BitBtnProjectInfoDelClick(Sender: TObject);
begin
  with vleProjectInfo do
  begin
    DeleteRow(vPRow);
  end;
end;

procedure TProjectInfoForm.vleProjectInfoSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  vPRow:= aRow;
end;

procedure TProjectInfoForm.SetProjectInfo;
var
  i:integer;
begin
  with ProjectInfo do
  begin
    ProjectName        := lbProjectName.Text;
    ProjectID          := lbProjectID.Text;
    ProjectDescription := lbDescripition.Text;
    ProjectPhase       := lbProjectPhase.Text;
    KeyWords           := lbKeyWords.Text;
    Notes:='';
    for i:=0 to memNotes.Lines.Count-1 do
    begin
      Notes:= Notes+ memNotes.Lines.Strings[i]+#10;
    end;
    TemplateAuthor     := lbTemplateAuthor.Text;
    ClearCustomParameters;
    with vleProjectInfo do
    begin
      for i:=0 to Strings.Count-1 do
      begin
        AddCustomParameter(TArchParameter.New(Cells[0,i],Cells[1,i]));
      end;
    end;
  end;
  SetSiteInfo;
  SetBuildingInfo;
  SetDesignerInfo;
  SetCustomerInfo;
end;

{-----------------------------------------------------------------------------
                             Site Info
-----------------------------------------------------------------------------}
procedure TProjectInfoForm.LoadSiteInfo(ASInfo: TArchSiteInfo);
var
  i:integer;
  strAux:AnsiString;
begin
  with ASInfo do
  begin
    lbSiteID.Text   := SiteID;
    lbSiteName.Text := SiteName;
    lbSiteArea.Text := FloatToStr(SiteArea);
    for i:= 1 to Length(SiteFullAddress) do
    begin
      if SiteFullAddress[i] = #92 then
      begin
        strAux:= strAux + #13#10;
      end else
      begin
        strAux:= strAux + SiteFullAddress[i];
      end;
    end;
    memSiteFullAdress.Lines.Add(strAux);
    memSiteDescripition.Lines.Add(SiteDescription);
    with vleSiteInfo do
    begin
      RowCount:=0;
      for i:= 0 to Length(SiteCustomParameters)-1 do
      begin
        InsertRow(SiteCustomParameters[i].ParamName,SiteCustomParameters[i].Value, true );
      end;
    end;
  end;
end;

procedure TProjectInfoForm.vleSiteInfoSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  vSRow:= aRow;
end;

procedure TProjectInfoForm.BitBtnSiteInfoAddClick(Sender: TObject);
begin
  vleSiteInfo.InsertRow('','',true);
end;

procedure TProjectInfoForm.BitBtnSiteInfoDelClick(Sender: TObject);
begin
  vleSiteInfo.DeleteRow(vSRow);
end;

procedure TProjectInfoForm.SetSiteInfo;
var
  i:Integer;
  strAux:String;
begin
  with ProjectInfo.Siteinfo do
  begin
    SiteID    := lbSiteID.Text;
    SiteName  := lbSiteName.Text;
    SiteArea  := StrToFloat(lbSiteArea.Text);
    for i:= 0 to memSiteFullAdress.Lines.Count do
    begin
      strAux:= memSiteFullAdress.Lines.Strings[i] + '\';
    end;
    SiteDescription:='';
    for i:=0 to memNotes.Lines.Count-1 do
    begin
      SiteDescription:= SiteDescription + memSiteDescripition.Lines.Strings[i]+#10;
    end;
    ClearCustomParameters;
    with vleSiteInfo do
    begin
      for i:=0 to Strings.Count-1 do
      begin
        AddCustomParameter(TArchParameter.New(Cells[0,i],Cells[1,i]));
      end;
    end;
  end;
end;

procedure TProjectInfoForm.lbSiteAreaExit(Sender: TObject);
var
  d:Double;
begin
  if not TryStrToFloat(lbSiteArea.Text,d) then
  begin
    lbSiteArea.SetFocus;
    ShowMessage(M_NONUMERIC_ALERT[ActiveLanguage]);
    lbSiteArea.Text:='0';
  end;
end;

{-----------------------------------------------------------------------------
                             Building Info
-----------------------------------------------------------------------------}
procedure TProjectInfoForm.LoadBuildingInfo(ABInfo: TArchBuildingInfo);
var
  i:integer;
begin
  with ABInfo do
  begin
    lbBuildingID.Text := BuildingID;
    lbBuildingName.Text:= BuildingName;
    memBuildingDescripition.Lines.Add(BuildingDescription);
    with vleBuildingInfo do
    begin
      RowCount:=0;
      for i:= 0 to Length(BuildingCustomParameters)-1 do
      begin
        InsertRow(BuildingCustomParameters[i].ParamName,BuildingCustomParameters[i].Value, true );
      end;
    end;
  end;
end;

procedure TProjectInfoForm.BitBtnBuildingInfoAddClick(Sender: TObject);
begin
  vleBuildingInfo.InsertRow('','',true);
end;

procedure TProjectInfoForm.BitBtnBuildingInfoDelClick(Sender: TObject);
begin
  vleBuildingInfo.DeleteRow(vBRow);
end;

procedure TProjectInfoForm.SetBuildingInfo;
var
  i:integer;
begin
  with ProjectInfo.BuildingInfo do
  begin
    BuildingID          := lbBuildingID.Text;
    BuildingName        := lbBuildingName.Text;
    BuildingDescription:='';
    for i:=0 to memBuildingDescripition.Lines.Count-1 do
    begin
      BuildingDescription:= BuildingDescription + memBuildingDescripition.Lines.Strings[i]+#10;
    end;
    ClearCustomParameters;
    with vleBuildingInfo do
    begin
      for i:=0 to Strings.Count-1 do
      begin
        AddCustomParameter(TArchParameter.New(Cells[0,i],Cells[1,i]));
      end;
    end;
  end;
end;

procedure TProjectInfoForm.vleBuildingInfoSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  vBRow:= aRow;
end;


{-----------------------------------------------------------------------------
                             Designer Info
-----------------------------------------------------------------------------}
procedure TProjectInfoForm.LoadDesignerInfo(ADInfo: TArchDesignerInfo);
var
  i:integer;
begin
  with ADInfo do
  begin
    lbDesignerID.Text   := DesignerIDRegister;
    lbDesignerName.Text := DesignerName;
    with vleDesignerInfo do
    begin
      RowCount:=0;
      for i:= 0 to Length(DesignerCustomParameters)-1 do
      begin
        InsertRow(DesignerCustomParameters[i].ParamName, DesignerCustomParameters[i].Value, true );
      end;
    end;
  end;
end;

procedure TProjectInfoForm.BitBtnDesignerInfoAddClick(Sender: TObject);
begin
  vleDesignerInfo.InsertRow('','',true);
end;

procedure TProjectInfoForm.BitBtnDesignerInfoDelClick(Sender: TObject);
begin
  vleDesignerInfo.DeleteRow(vDRow);
end;

procedure TProjectInfoForm.vleDesignerInfoSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  vDRow:= aRow;
end;

procedure TProjectInfoForm.SetDesignerInfo;
var
  i:integer;
begin
  with ProjectInfo.DesignerInfo do
  begin
    DesignerIDRegister := lbDesignerID.Text;
    DesignerName       := lbDesignerName.Text;
    ClearCustomParameters;
    with vleDesignerInfo do
    begin
      for i:=0 to Strings.Count-1 do
      begin
        AddCustomParameter(TArchParameter.New(Cells[0,i],Cells[1,i]));
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------------
                             Customer Info
-----------------------------------------------------------------------------}
procedure TProjectInfoForm.LoadCustomerInfo(ACInfo: TArchCustomerInfo);
var
  i:integer;
begin
  with ACInfo do
  begin
    lbCustomerID.Text   := CustomerID;
    lbCustomerName.Text := CustomerName;
    lbCustomerEmail.Text:= CustomerEmail;
    with vleCustomerInfo do
    begin
      RowCount:=0;
      for i:= 0 to Length(CustomerCustomParameters)-1 do
      begin
        InsertRow(CustomerCustomParameters[i].ParamName, CustomerCustomParameters[i].Value, true );
      end;
    end;
  end;
end;

procedure TProjectInfoForm.BitBtnCustomerInfoAddClick(Sender: TObject);
begin
  vleCustomerInfo.InsertRow('','',true);
end;

procedure TProjectInfoForm.vleCustomerInfoSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  vCRow:= aRow;
end;

procedure TProjectInfoForm.BitBtnCustomerInfoDelClick(Sender: TObject);
begin
  vleCustomerInfo.DeleteRow(vCRow);
end;

procedure TProjectInfoForm.SetCustomerInfo;
var
  i:integer;
begin
  with ProjectInfo.CustomerInfo do
  begin
    CustomerID   := lbCustomerID.Text;
    CustomerName := lbCustomerName.Text;
    CustomerEmail:= lbCustomerEmail.Text;
    ClearCustomParameters;
    with vleCustomerInfo do
    begin
      for i:= 0 to Strings.Count-1 do
      begin
        AddCustomParameter(TArchParameter.New(Cells[0,i],Cells[1,i]));
      end;
    end;
  end;
end;


{------------------------------------------------------------------------------
                         BtnOK
-------------------------------------------------------------------------------}
procedure TProjectInfoForm.btnCancelClick(Sender: TObject);
begin
  if MessageDlg(Q_DISCARD_CHANGES[ActiveLanguage],mtConfirmation,[mbYes,mbNo],0) = mrNo then
  begin
    Abort;
  end
    else Close;
end;

procedure TProjectInfoForm.btnOKClick(Sender: TObject);
begin
  SetProjectInfo;
  Close;
end;



end.

