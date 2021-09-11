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
 2021 04 17 - ET - Unit implementation Start's.
}

unit OpenArchLayersForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, EditBtn, ComboEx, Grids, DividerBevel, LvlGraphCtrl,
  //OpenArch:
  OpenArchLayer, Types;

type

  { TLayersForm }

  TLayersForm = class(TForm)
    btnSellectAll: TBitBtn;
    btnLockedAll: TBitBtn;
    btnVisibleSelect: TBitBtn;
    btnPrintableSelect: TBitBtn;
    btnCancelSellect: TBitBtn;
    btnCancelLockedSelect: TBitBtn;
    btnCancelVisibleSelect: TBitBtn;
    btnCancelPrintableSelect: TBitBtn;
    btnCancel: TButton;
    btnOK: TButton;
    btnNewLayersGroup: TButton;
    btnUpdateLayersGroup: TButton;
    btnDelLayersGroup: TButton;
    btnAddLayer: TButton;
    btnDelLayer: TButton;
    btnEditLayerGroup: TButton;
    cbboxLayerSet: TComboBox;
    labLayersGroup: TLabel;
    pnLayersGroup: TPanel;
    pnLayers: TPanel;
    pnBotton: TPanel;
    stgLayers: TStringGrid;
    procedure btnAddLayerClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnCancelLockedSelectClick(Sender: TObject);
    procedure btnCancelPrintableSelectClick(Sender: TObject);
    procedure btnCancelSellectClick(Sender: TObject);
    procedure btnCancelVisibleSelectClick(Sender: TObject);
    procedure btnDelLayerClick(Sender: TObject);
    procedure btnDelLayersGroupClick(Sender: TObject);
    procedure btnEditLayerGroupClick(Sender: TObject);
    procedure btnLockedAllClick(Sender: TObject);
    procedure btnNewLayersGroupClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnPrintableSelectClick(Sender: TObject);
    procedure btnSellectAllClick(Sender: TObject);
    procedure btnUpdateLayersGroupClick(Sender: TObject);
    procedure btnVisibleSelectClick(Sender: TObject);
    procedure cbboxLayerSetChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure stgLayersDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure stgLayersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure stgLayersSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure stgLayersValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    procedure LoadLayers(ALayers:TArchLayers);
    procedure LoadLayerSet(ALayerSet:TArchLayersSet);
    procedure BtnCaptions();
    procedure SetBoolValues(aCol, aRow:integer);
    procedure SetLockedLayers(ALayerGroupName:string);
    procedure SetVisibleLayers(ALayerGroupName:string);
    procedure SetPrintableLayers(ALayerGroupName:string);
    function GetLockedsLayers():TStringList;
    function GetVisiblesLayers():TStringList;
    function GetPrintablesLayers():TStringList;
  public
    constructor Create(TheOwner: TComponent;ALayers:TArchLayers;
      ALayersSet:TArchLayersSet); overload;
    function CheckNameExist(AName:String):boolean;
    function CheckNameEditExist(AName:String):boolean;
    function CheckSetLayerGroupNameExist(AName:String):boolean;
  end;

var
  LayersForm: TLayersForm;
  ProjectLayers,vLayers: TArchLayers;
  ProjectLayerSet,vLayersSet: TArchLayersSet;
  rowsSelected: array of integer;

implementation
uses
  OpenArchSysParameters;

{$R *.frm}

{ TLayersForm }

procedure TLayersForm.btnSellectAllClick(Sender: TObject);
var
  a:Boolean;
  i,j:integer;
begin
  SetLength(rowsSelected,0);
  for i:= 2 to stgLayers.RowCount -1 do
  begin
    SetLength(rowsSelected, (Length(rowsSelected)+1));
    rowsSelected[Length(rowsSelected)-1]:= i;
  end;
  stgLayers.SetFocus;
  stgLayers.Refresh;
end;

procedure TLayersForm.btnUpdateLayersGroupClick(Sender: TObject);
var
  i:integer;
begin
  vLayersSet.LayerGroupByName[cbboxLayerSet.Caption].DelAllLockeds();
  vLayersSet.LayerGroupByName[cbboxLayerSet.Caption].DelAllPrintables();
  vLayersSet.LayerGroupByName[cbboxLayerSet.Caption].DelAllVisibles();
  for i:=0 to stgLayers.RowCount-1 do
  begin
    if stgLayers.Cells[0,i]='T'then
      vLayersSet.LayerGroupByName[cbboxLayerSet.Caption].Lockeds.Add(stgLayers.Cells[3,i]);
    if stgLayers.Cells[1,i]='T' then
      vLayersSet.LayerGroupByName[cbboxLayerSet.Caption].Visibles.Add(stgLayers.Cells[3,i]);
    if stgLayers.Cells[2,i]='T' then
      vLayersSet.LayerGroupByName[cbboxLayerSet.Caption].Printables.Add(stgLayers.Cells[3,i]);
  end;
  btnUpdateLayersGroup.Enabled:=False;
  btnOK.Enabled:=True;
end;

procedure TLayersForm.btnVisibleSelectClick(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Length(rowsSelected)-1 do
  begin
    stgLayers.Cells[1,rowsSelected[i]]:= 'T';
  end;
end;

procedure TLayersForm.cbboxLayerSetChange(Sender: TObject);
var i:Int64;
begin
  if vLayersSet.LayerGroupByName[cbboxLayerSet.Text].Lockeds.Strings[0]=ARCH_ALL then
  begin
    for i:=0 to stgLayers.RowCount-1 do
    begin
      stgLayers.Cells[0,i]:='T';
    end;
  end else SetLockedLayers(cbboxLayerSet.Text);
  if vLayersSet.LayerGroupByName[cbboxLayerSet.Text].Visibles.Strings[0]=ARCH_ALL then
  begin
    for i:=0 to stgLayers.RowCount-1 do
    begin
      stgLayers.Cells[1,i]:='T';
    end;
  end else SetVisibleLayers(cbboxLayerSet.Text);
  if vLayersSet.LayerGroupByName[cbboxLayerSet.Text].Printables.Strings[0]=ARCH_ALL then
  begin
    for i:=0 to stgLayers.RowCount-1 do
    begin
      stgLayers.Cells[2,i]:='T';
    end;
  end else SetPrintableLayers(cbboxLayerSet.Text);
end;

procedure TLayersForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  vLayers.Free;
  vLayersSet.Free;
end;

procedure TLayersForm.stgLayersDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i:integer;
begin
  for i:= 0 to (Length(rowsSelected) - 1) do
  begin
    if rowsSelected[i]= aRow then
    begin
      stgLayers.Canvas.Brush.Color:=clActiveCaption;
    end;
  end;
  stgLayers.canvas.fillRect(aRect);
  if aRow=0 then
    stgLayers.canvas.TextOut(aRect.Left,aRect.Top,stgLayers.Columns[aCol].Title.Caption)
  else
    stgLayers.canvas.TextOut(aRect.Left,aRect.Top,stgLayers.Cells[ACol,ARow])
end;

procedure TLayersForm.btnCancelSellectClick(Sender: TObject);
begin
  SetLength(rowsSelected,0);
  stgLayers.SetFocus;
  stgLayers.Refresh;
end;

procedure TLayersForm.btnCancelVisibleSelectClick(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Length(rowsSelected)-1 do
  begin
    stgLayers.Cells[1,rowsSelected[i]]:= 'F';
  end;
end;

procedure TLayersForm.btnDelLayerClick(Sender: TObject);
var
  i,n:integer;
begin
  for i:= 0 to Length(rowsSelected)-1 do
  begin
    vLayers.DelLayer(stgLayers.Cells[3,rowsSelected[i]]);
  end;
  LoadLayers(vLayers);
  n:=rowsSelected[Length(rowsSelected)-1];
  SetLength(rowsSelected,1);
  rowsSelected[0]:=n;
  stgLayers.Selection:=TGridRect(Rect(0,rowsSelected[0],3,rowsSelected[0]));
  stgLayers.Row:=rowsSelected[0];
  stgLayers.Col:=3;
end;

procedure TLayersForm.btnDelLayersGroupClick(Sender: TObject);
begin
    vLayersSet.DelLayerGroup(cbboxLayerSet.Caption);
    LoadLayerSet(vLayersSet);
end;

procedure TLayersForm.btnEditLayerGroupClick(Sender: TObject);
var
  vLayerName:String;
begin
  if InputQuery('Editar Nome do Conjunto','Novo Nome:',vLayerName) then
  begin
    if CheckSetLayerGroupNameExist(vLayerName) then
    begin
      ShowMessage('Nome do Conjunto já Existe!')
    end else
    begin
      vLayersSet.EditLayerGroupName(cbboxLayerSet.Caption,vLayerName);
      LoadLayerSet(vLayersSet);
    end;
  end;
end;

procedure TLayersForm.btnCancelLockedSelectClick(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Length(rowsSelected)-1 do
  begin
    stgLayers.Cells[0,rowsSelected[i]]:= 'F';
  end;
end;

procedure TLayersForm.btnAddLayerClick(Sender: TObject);
var
  vLayerName:String;
  vLayer:TArchLayer;
begin
  if InputQuery('Nova Camada','Nome da Camada',vLayerName) then
  begin
    if CheckNameExist(vLayerName) then
    begin
      ShowMessage('Nome da Camada já Existe!');
    end else
    begin
      vLayer:= TArchLayer.Create(vLayerName,False,False,False);
      vLayers.AddLayer(vLayer);
      LoadLayers(vLayers);
      stgLayers.SortColRow(True,3);
    end;
  end;
end;

procedure TLayersForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TLayersForm.btnCancelPrintableSelectClick(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Length(rowsSelected)-1 do
  begin
    stgLayers.Cells[2,rowsSelected[i]]:= 'F';
  end;
end;

procedure TLayersForm.btnLockedAllClick(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Length(rowsSelected)-1 do
  begin
    stgLayers.Cells[0,rowsSelected[i]]:= 'T';
  end;
end;

procedure TLayersForm.btnNewLayersGroupClick(Sender: TObject);
var
  vLayerGroupName:String;
  i:integer;
begin
  if InputQuery('Novo COnjunto de Camadas','Nome do Conjunto:',vLayerGroupName) then
  begin
    if CheckSetLayerGroupNameExist(vLayerGroupName) then
      ShowMessage('Nome do Conjunto já Existe!')
      else
    begin
      vLayersSet.AddLayerGroup(vLayerGroupName,
                               GetVisiblesLayers(),
                               GetLockedsLayers(),
                               GetPrintablesLayers());
      LoadLayerSet(vLayersSet);
      cbboxLayerSet.ItemIndex:=cbboxLayerSet.Items.IndexOf(vLayerGroupName);
    end;
  end;
end;

procedure TLayersForm.btnOKClick(Sender: TObject);
begin
  ProjectLayerSet.Clone(vLayersSet);
  ProjectLayers.Clone(vLayers);
  Close;
end;

procedure TLayersForm.btnPrintableSelectClick(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Length(rowsSelected)-1 do
  begin
    stgLayers.Cells[2,rowsSelected[i]]:= 'T';
  end;
end;

procedure TLayersForm.stgLayersMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vCol,vRow: LongInt;
begin
  stgLayers.MouseToCell(X,Y,vCol, vRow);
  if not (stgLayers.Cells[3,vRow]='_Camada OpenArch') then
  begin
    SetBoolValues(vCol,vRow);
  end;
  if ssCtrl in Shift then
  begin
    SetLength(rowsSelected,(Length(rowsSelected)+1));
    rowsSelected[Length(rowsSelected)-1]:= vRow;
  end else
  begin
    SetLength(rowsSelected,1);
    rowsSelected[0]:=vRow;
  end;
end;

procedure TLayersForm.stgLayersSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (stgLayers.Cells[3,aRow]= '_Camada OpenArch') then CanSelect:=False;
end;

procedure TLayersForm.stgLayersValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var
  i:integer;
begin
  if aCol=3 then
  begin
    if (NewValue<>OldValue) then
        begin
          if (CheckNameEditExist(NewValue)) then
          begin
            ShowMessage('Nome da Camada Já Existe!');
            NewValue:=OldValue;
            stgLayers.Col:=3;
            stgLayers.Row:=aRow;
          end else
          begin
            with stgLayers do
            begin
              vLayers.EditLayer(OldValue,
                                NewValue,
                                Cells[0,aRow],
                                Cells[1,aRow],
                                Cells[2,aRow]);
            end;
            LoadLayers(vLayers);
          end;
        end;
  end;
  btnUpdateLayersGroup.Enabled:=true;
end;

procedure TLayersForm.LoadLayers(ALayers: TArchLayers);
var
  i:integer;
begin
  stgLayers.RowCount:= ALayers.LayersCount+1;
  for i:= 0 to ALayers.LayersCount-1 do
  begin
    with ALayers do
    begin
      if Layer[i].Locked then stgLayers.Cells[0,i+1]:= 'T'
        else stgLayers.Cells[0,i+1]:= 'F';
      if Layer[i].Visible then stgLayers.Cells[1,i+1]:= 'T'
        else stgLayers.Cells[1,i+1]:= 'F';
      if Layer[i].Printable then stgLayers.Cells[2,i+1]:= 'T'
        else stgLayers.Cells[2,i+1]:= 'F';
      stgLayers.Cells[3,i+1]:=Layer[i].LayerName;
    end;
  end;
  stgLayers.SortColRow(true,3);
end;

procedure TLayersForm.LoadLayerSet(ALayerSet: TArchLayersSet);
var
  i: Int64;
begin
  cbboxLayerSet.Clear;
  for i:=0 to ALayerSet.LayerSetCount - 1 do
  begin
    cbboxLayerSet.Items.Add(ALayerSet.LayerGroup[i].LayerGroupName);
  end;
  cbboxLayerSet.ItemIndex:=0;
end;

procedure TLayersForm.BtnCaptions();
begin
  btnSellectAll.Caption            := 'Sel.'+ LineEnding +' Todos';
  btnCancelSellect.Caption         := 'Cancel'+LineEnding+' Sel.';
  btnLockedAll.Caption             := 'Block'+LineEnding+' Sel.';
  btnCancelLockedSelect.Caption    := 'Cancel'+LineEnding+' Block';
  btnVisibleSelect.Caption         := LineEnding+' Visível';
  btnCancelVisibleSelect.Caption   := 'Ocultar';
  btnPrintableSelect.Caption       := LineEnding+' Impm';
  btnCancelPrintableSelect.Caption := LineEnding+' N Impm';
end;

procedure TLayersForm.SetBoolValues(aCol,aRow:integer);
begin
  case aCol of
    0,1,2 :
      begin
        with stgLayers do
        begin
          if Cells[aCol,aRow]='T' then Cells[aCol,aRow]:= 'F'
            else Cells[aCol,aRow]:='T';
          Options:=[goFixedVertLine,goRowSelect,goRangeSelect,goSmoothScroll];
          ColumnClickSorts:=false;
          vLayers.EditLayer(Cells[3,aRow],
                            Cells[3,aRow],
                            Cells[0,aRow],
                            Cells[1,aRow],
                            Cells[2,aRow]);
        end;
      end;
  end;
  btnUpdateLayersGroup.Enabled:=true;
  btnOK.Enabled:=false;
end;

procedure TLayersForm.SetLockedLayers(ALayerGroupName:string);
var i,j:int64;
begin
  for i:= 0 to stgLayers.RowCount-1 do
  begin
    for j:= 0 to  vLayersSet.LayerGroupByName[ALayerGroupName].Lockeds.Count-1 do
    begin
      if (stgLayers.Cells[3,i]=vLayersSet.LayerGroupByName[ALayerGroupName].Lockeds.Strings[J]) then
      begin
        stgLayers.Cells[0,i]:='T';
        Break;
      end else
      begin
        stgLayers.Cells[0,i]:='F';
      end;
    end;
  end;
end;

procedure TLayersForm.SetVisibleLayers(ALayerGroupName:string);
var i,j:int64;
begin
  for i:= 0 to stgLayers.RowCount-1 do
  begin
    for j:= 0 to  vLayersSet.LayerGroupByName[ALayerGroupName].Visibles.Count-1 do
    begin
      if (stgLayers.Cells[3,i]=vLayersSet.LayerGroupByName[ALayerGroupName].Visibles.Strings[J]) then
      begin
        stgLayers.Cells[1,i]:='T';
        Break;
      end else
      begin
        stgLayers.Cells[1,i]:='F';
      end;
    end;
  end;
end;

procedure TLayersForm.SetPrintableLayers(ALayerGroupName:string);
var i,j:int64;
begin
  for i:= 0 to stgLayers.RowCount-1 do
  begin
    for j:= 0 to  vLayersSet.LayerGroupByName[ALayerGroupName].Printables.Count-1 do
    begin
      if (stgLayers.Cells[3,i]=vLayersSet.LayerGroupByName[ALayerGroupName].Printables.Strings[J]) then
      begin
        stgLayers.Cells[2,i]:='T';
        Break;
      end else
      begin
        stgLayers.Cells[2,i]:='F';
      end;
    end;
  end;
end;

function TLayersForm.GetLockedsLayers(): TStringList;
var
  i:integer;
  list:TStringList;
begin
  list:= TStringList.Create;
  for i:= 0 to stgLayers.RowCount - 1 do
  begin
    if stgLayers.Cells[0,i]='T' then
    begin
      list.Add(stgLayers.Cells[3,i]);
    end;
  end;
  Result:= list;
end;

function TLayersForm.GetVisiblesLayers(): TStringList;
var
  i:integer;
  list:TStringList;
begin
  list:= TStringList.Create;
  for i:= 0 to stgLayers.RowCount - 1 do
  begin
    if stgLayers.Cells[1,i]='T' then
      list.Add(stgLayers.Cells[3,i]);
  end;
  Result:= list;
end;

function TLayersForm.GetPrintablesLayers(): TStringList;
var
  i:integer;
  list:TStringList;
begin
  list:= TStringList.Create;
  for i:= 0 to stgLayers.RowCount - 1 do
  begin
    if stgLayers.Cells[2,i]='T' then
      list.Add(stgLayers.Cells[3,i]);
  end;
  Result:= list;
end;

constructor TLayersForm.Create(TheOwner: TComponent; ALayers: TArchLayers;
  ALayersSet: TArchLayersSet);
begin
  Inherited Create(TheOwner);
  ProjectLayers:= ALayers;
  ProjectLayerSet:= ALayersSet;
  vLayers:= TArchLayers.Create;
  vLayers.Clone(ProjectLayers);
  vLayersSet:= TArchLayersSet.Create;
  vLayersSet.Clone(ProjectLayerSet);
  LoadLayers(vLayers);
  LoadLayerSet(vLayersSet);
  BtnCaptions();
  btnUpdateLayersGroup.Enabled:=false;
  SetLength(rowsSelected,1);
  rowsSelected[0]:=2;
end;

function TLayersForm.CheckNameExist(AName:String):boolean;
var
  i:integer;
begin
  try
     for i:= 0 to vLayers.LayersCount-1 do
     begin
       if (vLayers.Layer[i].LayerName= AName)then
         begin
           Result:= true;
           Break;
         end else
         begin
           Result:= False;
         end;
     end;
  except
    Result:= false;
  end;
end;

function TLayersForm.CheckNameEditExist(AName: String): boolean;
var
  i,n:integer;
  vChecked: Boolean;
begin
  try
     n:=0;
     i:=0;
     repeat
       if (stgLayers.Cells[3,n]= AName)then
         begin
           i:=i+1;
           n:= n+1;
           if i=2 then
           begin
             Result:= True;
             vChecked:=true;
           end;
         end
         else if (n= stgLayers.RowCount-1) then
              begin
                Result:= false;
                vChecked:=true;
              end
              else
              begin
                n:= n+1;
                vChecked:=False;
              end;
     until vChecked;
  except
    Result:= false;
  end;
end;

function TLayersForm.CheckSetLayerGroupNameExist(AName: String): boolean;
var
  i:integer;
begin
  i:=0;
  try
   for i:= 0 to cbboxLayerSet.Items.Count - 1 do
   begin
     if (cbboxLayerSet.Items.Strings[i]= AName) then
     begin
       Result:= True;
       Break;
     end else
     begin
       Result:= False;
     end;
   end;
  except
    Result:= false;
  end;
end;

end.

