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
 2021 05 06 - ET - Unit implementation Start's.
}

unit OpenArchLineTypesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ComboEx, Grids, TplComboBoxPenStyleUnit,
  TplComboBoxPenWidthUnit,
  // GLScene:
  GL,GLScene, GLViewer, GLObjects, GLTexture, GLRenderContextInfo,
  //OpenArch:
  OpenArchGraphicResources, Types;

type

  { TLineTypesForm }

  TLineTypesForm = class(TForm)
    btnOK: TButton;
    btnDelete: TButton;
    btnAddLineType: TButton;
    btnRename: TButton;
    btnAddDash: TButton;
    btnDelDash: TButton;
    ComboBoxEx1: TComboBoxEx;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLLines1: TGLLines;
    GLMemoryViewer1: TGLMemoryViewer;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    stgDash: TStringGrid;
    procedure btnAddDashClick(Sender: TObject);
    procedure btnDelDashClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddLineTypeClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure ComboBoxEx1Change(Sender: TObject);
    procedure ShowLineType(AIndex:integer);
    procedure EnableDash;
    procedure DisableDash;
    procedure stgDashMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure stgDashSelection(Sender: TObject; aCol, aRow: Integer);
    procedure stgDashValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private

  public
    LineTypes:TArchLineTypes;
    constructor Create(TheOwner: TComponent; ALineTypes:TArchLineTypes); overload;
    procedure LoadLineTypes;
    procedure LoadDashAndGaps(AIndex:integer);


  end;

var
  LineTypesForm: TLineTypesForm;
  lineCount, colSelected:integer;


implementation
uses
  OpenArchLine, OpenArchDialogs;

{$R *.frm}

{ TLineTypesForm }

procedure TLineTypesForm.btnDeleteClick(Sender: TObject);
begin
  LineTypes.DelLineType(ComboBoxEx1.
                        ItemsEx.
                        Items[ComboBoxEx1.
                              ItemIndex].
                        Caption);
  LoadLineTypes;
end;

procedure TLineTypesForm.btnAddDashClick(Sender: TObject);
var
  i:integer;
  archLine:TArchLine;
begin
  archLine:= GLScene1.Objects.Children[2] as TArchLine;
  i:= ComboBoxEx1.ItemIndex;
  LineTypes.LineTypeByName[ComboBoxEx1.
                                   ItemsEx.
                                   Items[ComboBoxEx1.
                                         ItemIndex].
                                   Caption].AddDashed(0,0.02);
  LoadLineTypes;
  archLine.SetLineType(LineTypes.LineType[i]);
  ShowLineType(i);
end;

procedure TLineTypesForm.btnDelDashClick(Sender: TObject);
var
  i:integer;
  archLine:TArchLine;
begin
  archLine:= GLScene1.Objects.Children[2] as TArchLine;
  i:= ComboBoxEx1.ItemIndex;
  LineTypes.LineTypeByName[ComboBoxEx1.
                                   ItemsEx.
                                   Items[ComboBoxEx1.
                                         ItemIndex].
                                   Caption].DelDashed(colSelected-1);
  LoadLineTypes;
  archLine.SetLineType(LineTypes.LineType[i]);
  ShowLineType(i);
end;

procedure TLineTypesForm.btnAddLineTypeClick(Sender: TObject);
var
  vReturns:array of string;
begin
  SetLength(vReturns,2);
  if (InputDialogRadioGroup('Novo Tipo de Linha',
                            ['Nome','Estilo da Linha'],
                            [1,0],
                            ['Contínua','Tracejada'],
                            vReturns)) then
  begin
    if LineTypes.CheckLineTypeNameExist(vReturns[0]) then
    begin
      ShowMessage('Nome da Linha já Existe!');
      ShowLineType(0);
    end else
    begin
      if vReturns[1]='Contínua' then
      begin
        LineTypes.AddLineType(vReturns[0],lsSolid);
      end else
      begin
        LineTypes.AddLineType(vReturns[0],lsDashed);
      end;
      LoadLineTypes;
      ShowLineType(ComboBoxEx1.Items.Count-1);
    end;
  end else
  begin
    ShowLineType(0);
  end;
end;

procedure TLineTypesForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TLineTypesForm.btnRenameClick(Sender: TObject);
var
  vLineTypeName:String;
begin
  if InputQuery('Renomear Tipo de Linha','Nome:',vLineTypeName) then
  begin
    if LineTypes.CheckLineTypeNameExist(vLineTypeName) then
      ShowMessage('Nome da Linha já Existe!')
    else LineTypes.LineTypeByName[ComboBoxEx1.
                                  ItemsEx.
                                  Items[ComboBoxEx1.
                                        ItemIndex].
                                  Caption].
                                  LineTypeName:=vLineTypeName;
  end;
  LoadLineTypes;
end;

procedure TLineTypesForm.ComboBoxEx1Change(Sender: TObject);
var
  archLine:TArchLine;
begin
  archLine:= GLScene1.Objects.Children[2] as TArchLine;
  archLine.SetLineType(LineTypes.LineType[ComboBoxEx1.ItemIndex]);
  ShowLineType(ComboBoxEx1.ItemIndex);
end;

procedure TLineTypesForm.ShowLineType(AIndex: integer);
begin
  if (LineTypes.LineType[AIndex].LineStyle=lsSolid) then
  begin
    ComboBoxEx1.ItemIndex:=AIndex;
    LoadDashAndGaps(AIndex);
    DisableDash;
  end else
  begin
    ComboBoxEx1.ItemIndex:=AIndex;
    LoadDashAndGaps(AIndex);
    EnableDash;
  end;
end;

procedure TLineTypesForm.EnableDash;
begin
  btnAddDash.Enabled:=True;
  btnDelDash.Enabled:=True;
  stgDash.Enabled:=True;
end;

procedure TLineTypesForm.DisableDash;
begin
  btnAddDash.Enabled:=False;
  btnDelDash.Enabled:=False;
  stgDash.Enabled:=False;
end;

procedure TLineTypesForm.stgDashMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vCol,vRow:integer;
begin
  stgDash.MouseToCell(X,Y,vCol,vRow);
  colSelected:=vCol;
  stgDash.SelectedColumn;
end;

procedure TLineTypesForm.stgDashSelection(Sender: TObject; aCol, aRow: Integer);
var i:integer;
begin
  for i:=0 to stgDash.Columns.Count-1 do
  begin
    with stgDash do
    begin
      if i= colSelected-1 then Columns[i].Color:=cl3DLight
        else Columns[i].Color:=clDefault;
    end;
  end;
end;

procedure TLineTypesForm.stgDashValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var
  i:integer;
  archLine:TArchLine;
  eValue:single;
begin
  if TryStrToFloat(NewValue,eValue) then
  begin
    eValue:=eValue/1000;
    archLine:= GLScene1.Objects.Children[2] as TArchLine;
    i:= ComboBoxEx1.ItemIndex;
    if aRow=0 then
    begin
      LineTypes.LineTypeByName[ComboBoxEx1.
                               ItemsEx.
                               Items[ComboBoxEx1.
                                     ItemIndex].
                               Caption].
                               LineTypeDash[aCol-1]:=eValue;
    end else
    begin
      LineTypes.LineTypeByName[ComboBoxEx1.
                               ItemsEx.
                               Items[ComboBoxEx1.
                                     ItemIndex].
                               Caption].
                               LineTypeGap[aCol-1]:=eValue;
    end;
    LoadLineTypes;
    archLine.SetLineType(LineTypes.LineType[i]);
    ShowLineType(i);
  end else
  begin
    ShowMessage('Digite um valor numérico!');
  end;
end;

constructor TLineTypesForm.Create(TheOwner: TComponent;
  ALineTypes: TArchLineTypes);
var
  archLine:TArchLine;
begin
  inherited Create(TheOwner);
  LineTypes:= ALineTypes;
  GLScene1.Objects.AddChild(TArchLine.Create(GLScene1,LineTypes.LineType[6]));
  lineCount:=0;
  LoadLineTypes;
  archLine:= GLScene1.Objects.Children[2] as TArchLine;
  archLine.SetLineType(LineTypes.LineType[0]);
  ShowLineType(0);
  colSelected:=1;
end;

procedure TLineTypesForm.LoadLineTypes;
var
  i:integer;
  archLine:TArchLine;
  bmp: TBitmap;
begin
  ComboBoxEx1.ItemsEx.Clear;
  archLine:= GLScene1.Objects.Children[2] as TArchLine;
  archLine.SetStarPoint(-0.06,0,0);
  archLine.SetEndPoint(0.06,0,0);
  bmp:= TBitmap.Create;
  ImageList1.Height:=GLMemoryViewer1.Height;
  ImageList1.Width:=GLMemoryViewer1.Width;
  ImageList1.Clear;
  for i:= 0 to LineTypes.Count-1 do
  begin
    ComboBoxEx1.ItemsEx.AddItem(LineTypes.LineType[i].LineTypeName,i,i,i,i,nil);
    archLine.SetLineType(LineTypes.LineType[i]);
    bmp.SetSize(GLMemoryViewer1.Width ,GLMemoryViewer1.Height);
    GLMemoryViewer1.Render;
    GLMemoryViewer1.Buffer.RenderToBitmap(bmp,72);
    ImageList1.AddMasked(bmp,clWhite);
  end;
end;

procedure TLineTypesForm.LoadDashAndGaps(AIndex:integer);
var
  i:integer;
begin
  for i:= stgDash.ColCount-1 downto 1 do
    stgDash.DeleteCol(i);
  stgDash.Cells[0,0]:='Traço (mm):';
  stgDash.Cells[0,1]:='Vazio (mm):';
  for i:=0 to LineTypes.LineType[AIndex].DashesCount-1 do
  begin
    stgDash.Columns.Add;
    stgDash.Cells[i+1, 0]:=FormatFloat('#,##0.00',LineTypes.LineType[AIndex].LineTypeDash[i]*1000);
    stgDash.Cells[i+1, 1]:=FormatFloat('#,##0.00',LineTypes.LineType[AIndex].LineTypeGap[i]*1000);
  end;
end;

end.

