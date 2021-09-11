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
 2021 04 02 - ET - Finish the code of this form, preliminar tests ok!
 2021 02 19 - ET - Unit implementation Start's.
}



unit OpenArchStoriesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  ComCtrls, StdCtrls, Types,
  //OpenArch:
  OpenArchStory;

type

  { TStoriesForm }

  TStoriesForm = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnAddStoryDown: TButton;
    btnAddStoryUp: TButton;
    btnDelStory: TButton;
    pnBotton: TPanel;
    stgStories: TStringGrid;
    procedure btnAddStoryDownClick(Sender: TObject);
    procedure btnAddStoryUpClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDelStoryClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure stgStoriesCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure stgStoriesDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure stgStoriesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure stgStoriesSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure stgStoriesValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    procedure LoadStories();
    function RowInIndex(AStrIndex:string):integer;
    function GetNumOfStory(aRow:integer):integer;
    function GetNumOfStoryInList(aRow:integer):integer;
    function GetTopIndexInList():Integer;
    function GetBottomIndexInList():integer;
    function GetPositionStoryInList(ANumOfStory:integer):integer;
    function CheckAbove(APosition:integer):boolean;
    function CheckAboveOrEqualZero(aPosition:integer):Boolean;
    function CheckBelow(APosition:integer):boolean;
    procedure AdjustElevation(ARow:integer);
    procedure EditElevation(AEditCase, ARow:integer; AElevation:Double);
    procedure EditHeight(AEditCase, ARow:integer; AHeight:Double);
    {In this procedure, we send as parameter the 04 possible cases of Story insertion:
    1: Above Zero and Above the Selected Story;
    2: Below Zero and Below the Selected Story;
    3: Above Zero and Below the Selected Story;
    4: Below Zero and Above the Selected Story:}
    procedure AddStory(AName:string; InsertionCase:integer);
    procedure LoadStringGrid();
  public
    constructor Create(TheOwner: TComponent;AStories:TArchStories); overload;
  end;

var
  StoriesForm: TStoriesForm;
  ProjectStories: TArchStories;
  rowSelected: Int64;
  ListOfStories: array of TStringList;

implementation
uses
  OpenArchDialogs;

{$R *.frm}

{ TStoriesForm }

procedure TStoriesForm.stgStoriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vCol,vRow: LongInt;
begin
  stgStories.MouseToCell(X,Y,vCol, vRow);
  rowSelected:=vRow;
end;

procedure TStoriesForm.stgStoriesSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  rowSelected:=aRow;
end;

procedure TStoriesForm.stgStoriesValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var
  ActualElevation, ActualHeight: Double;
  i,SelectedIndex:integer;
begin
  if aCol=1 then
  begin
    ListOfStories[GetPositionStoryInList(GetNumOfStory(aRow))].Strings[2]:= NewValue;
  end;
  //Capture the actual elevation, in case of the string typed by user,
  //put de zero valeu as default:
  if aCol=2 then
  begin
    ActualElevation:=StrToFloatDef(NewValue,0);
    {checks if the Elevation changed is the one on the highest Story and makes the modification:}
    if aRow=1 then i:=0
      else if aRow= stgStories.RowCount-1 then i:=1
             else i:=2;
    EditElevation(i,aRow,ActualElevation);
  end;
  if aCol=3 then
  begin
    ActualHeight:=StrToFloatDef(NewValue,0);
    SelectedIndex:=StrToInt(stgStories.Cells[0,aRow]);
    if CheckAboveOrEqualZero(SelectedIndex) then
    begin
      if aRow=1 then i:=0
        else i:=1;
    end else
    begin
      if aRow= stgStories.RowCount-1 then i:=3
        else i:= 2;
    end;
    EditHeight(i,aRow,ActualHeight);
  end;
end;

procedure TStoriesForm.btnAddStoryDownClick(Sender: TObject);
var
  vQuestions,vAnswers: array of string;
  SelectedIndex:Integer;
begin
  vQuestions:=['Nome do Pavto.'];
  SetLength(vAnswers,1);
  SelectedIndex:=StrToInt(stgStories.Cells[0,rowSelected]);
  if InputDialog('Novo Pavimento',vQuestions,[1],vAnswers) then
  begin
      //check if it's inserting above floor 0 to be able to increase the floor below or above
      if CheckAbove(SelectedIndex) then
      begin
        AddStory(vAnswers[0],3);
      end else
      begin
        AddStory(vAnswers[0],2);
      end;
  end;
end;

procedure TStoriesForm.btnAddStoryUpClick(Sender: TObject);
var
  vQuestions,vAnswers: array of string;
  SelectedIndex:Integer;
begin
  vQuestions:=['Nome do Pavto.'];
  SetLength(vAnswers,1);
  SelectedIndex:=StrToInt(stgStories.Cells[0,rowSelected]);
  if InputDialog('Novo Pavimento',vQuestions,[1],vAnswers) then
  begin
    with stgStories do
    begin
      //check if it's inserting above floor 0 to be able to increase the floor below or above
      if CheckBelow(SelectedIndex) then
      begin
        AddStory(vAnswers[0],4);
      end else
      begin
        AddStory(vAnswers[0],1);
      end;
    end;
  end;
end;

procedure TStoriesForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TStoriesForm.btnDelStoryClick(Sender: TObject);
var
  auxIndexActual,auxIndexNext: string;
  startRepairIndex:boolean;
  listAux: array of TStringList;
  i,n, aux, listLength, numOfStory:integer;
begin
  numOfStory:= GetNumOfStory(rowSelected);
  if not (numOfStory=0) then
  begin
    SetLength(listAux,0);
    for i:= 0 to Length(ListOfStories)-1 do
    begin
      if not (GetNumOfStoryInList(i)=numOfStory) then
      begin
        SetLength(listAux,Length(listAux)+1);
        listAux[Length(listAux)-1]:= ListOfStories[i];
      end;
    end;
    SetLength(ListOfStories,Length(listAux));
    for i:=0 to Length(listAux)-1 do
    begin
      ListOfStories[i]:= listAux[i];
    end;
    if CheckAbove(numOfStory) then
    begin
      n:= 1;
      for i:= 0 to Length(ListOfStories)-1 do
      begin
        if (GetNumOfStoryInList(i)>0) then
        begin
          ListOfStories[i].Strings[1]:= IntToStr(n);
          n:=n+1
        end;
      end;
    end else
    begin
      n:= -1;
      for i:=0 to Length(ListOfStories)-1 do
      begin
        if (GetNumOfStoryInList(i)<0) then
        begin
          ListOfStories[i].Strings[1]:= IntToStr(n);
          n:=n-1;
        end;
      end;
    end;
    LoadStringGrid();
  end else
  begin
    ShowMessage('OpenArch não permite que seja apagado o Piso 0');
  end;
end;

procedure TStoriesForm.btnOKClick(Sender: TObject);
var
  i,j:integer;
  storyToAdd: TArchStory;
  vChecked:Boolean;
  listOfDeleted:TStringList;
begin
  TrueBoolStrs:= ['1'];
  listOfDeleted:= TStringList.Create;
  // Make the stories deletes:
  for i:=0 to ProjectStories.StoriesCount-1 do
  begin
    vChecked:=False;
    for j:=0 to Length(ListOfStories)-1 do
    begin
      if ProjectStories.Story[i].StoryID=ListOfStories[j].Strings[0] then
      begin
        vChecked:=True;
      end;
    end;
    if not vChecked then
    begin
      listOfDeleted.Add(ProjectStories.Story[i].StoryID);
    end;
  end;
  for i:=0 to ProjectStories.StoriesCount-1 do
  begin
    for j:=0 to listOfDeleted.Count-1 do
    begin
      if ProjectStories.Story[i].StoryID=listOfDeleted[j] then
      begin
        ProjectStories.DelStory(ProjectStories.Story[i].StoryID);
      end;
    end;
  end;
  // Make the Stories edit:
  for i:=0 to Length(ListOfStories)-1 do
  begin
    for j:=0 to ProjectStories.StoriesCount-1 do
    begin
      if ListOfStories[i].Strings[0]=ProjectStories.Story[j].StoryID then
      begin
        ProjectStories.Story[j].NumStory:=strtoint(ListOfStories[i].Strings[1]);
        ProjectStories.Story[j].StoryName:=ListOfStories[i].Strings[2];
        ProjectStories.Story[j].Elevation:=StrToFloatDef(ListOfStories[i].Strings[3],0);
        ProjectStories.Story[j].Height:=StrToFloatDef(ListOfStories[i].Strings[4],0);
        ProjectStories.Story[j].ShowInSectionAndElevation:=StrToBool(ListOfStories[i].Strings[5]);
      end;
    end;
  end;
  // Make the storries adds:
  for i:=0 to Length(ListOfStories)-1 do
  begin
    if ListOfStories[i].Strings[0]='' then
    begin
      storyToAdd:= TArchStory.Create(StrToFloatDef(ListOfStories[i].Strings[3],0),
                                     StrToFloatDef(ListOfStories[i].Strings[4],0),
                                     ListOfStories[i].Strings[2]);
      storyToAdd.ShowInSectionAndElevation:=StrToBool(ListOfStories[i].Strings[5]);
      storyToAdd.NumStory:=strtoint(ListOfStories[i].Strings[1]);
      ListOfStories[i].Strings[0]:= storyToAdd.StoryID;
      ProjectStories.AddStory(storyToAdd);
    end;
  end;
 // Close the form:
  //ProjectStories.RefreshStories();
  Close;
end;

procedure TStoriesForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
var i:integer;
begin
  for i:= Length(ListOfStories)-1 downto 0 do
  begin
    FreeAndNil(ListOfStories[i]);
  end;
end;

procedure TStoriesForm.stgStoriesCheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  if aCol=4 then
  begin
    ListOfStories[GetPositionStoryInList(GetNumOfStory(aRow))].Strings[4]:= stgStories.Cells[4,aRow];
  end;
end;

procedure TStoriesForm.stgStoriesDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, lRow:integer;
  StrIndex:string;
begin
  for i:= 0 to (Length(ListOfStories) - 1) do
  begin
    StrIndex:=ListOfStories[i].Strings[1];
    lRow:=RowInIndex(StrIndex);
    if (aRow=lRow) and (aCol=0) then
    begin
      if (ListOfStories[i].Strings[0]<>'') then
      begin
        stgStories.Canvas.Brush.Color:=$00F1DCE0;
      end;
      stgStories.canvas.fillRect(aRect);
    end;
  end;
  if aCol=0 then
    stgStories.canvas.TextOut(aRect.Left+5,aRect.Top,stgStories.Cells[0,ARow]);
end;

procedure TStoriesForm.LoadStories();
var
  i:integer;
begin
  stgStories.Clear;
  SetLength(ListOfStories,0);
  for i:=1 to ProjectStories.StoriesCount do
  begin
    SetLength(ListOfStories,i);
    ListOfStories[Length(ListOfStories)-1]:= TStringList.Create;
    ListOfStories[Length(ListOfStories)-1].Add(ProjectStories.Story[i-1].StoryID);
    ListOfStories[Length(ListOfStories)-1].Add(inttostr(ProjectStories.Story[i-1].NumStory));
    ListOfStories[Length(ListOfStories)-1].Add(ProjectStories.Story[i-1].StoryName);
    ListOfStories[Length(ListOfStories)-1].Add(FormatFloat('#,##0.00',ProjectStories.Story[i-1].Elevation));
    ListOfStories[Length(ListOfStories)-1].Add(FormatFloat('#,##0.00',ProjectStories.Story[i-1].Height));
    if (ProjectStories.Story[i-1].ShowInSectionAndElevation) then
      ListOfStories[Length(ListOfStories)-1].Add('1')
      else
      ListOfStories[Length(ListOfStories)-1].Add('0');
  end;
  LoadStringGrid();
end;

{returns the row (integer) where the index (in string) is sent}
function TStoriesForm.RowInIndex(AStrIndex:string): integer;
var
  i:integer;
  check:Boolean;
begin
  i:=0;
  repeat
    check:=(stgStories.Cells[0,i]=AStrIndex);
    if check then Result:= i;
    i:=i+1;
  until check;
end;

function TStoriesForm.GetNumOfStory(aRow: integer): integer;
begin
  Result:= StrToInt(stgStories.Cells[0,aRow]);
end;

function TStoriesForm.GetNumOfStoryInList(aRow: integer): integer;
begin
  Result:=StrToInt(ListOfStories[aRow].Strings[1]);
end;

function TStoriesForm.GetTopIndexInList(): Integer;
var
  i, j, vTop:integer;
begin
  vTop:=0;
  for i:=0 to Length(ListOfStories)-1 do
  begin
    j:=GetNumOfStoryInList(i);
    if j > vTop then vTop:= j;
  end;
  Result:= vTop;
end;

function TStoriesForm.GetBottomIndexInList(): integer;
var
  i, j, vBottom:integer;
begin
  vBottom:=0;
  for i:=0 to Length(ListOfStories)-1 do
  begin
    j:=GetNumOfStoryInList(i);
    if j < vBottom then vBottom:= j;
  end;
  Result:= vBottom;
end;

function TStoriesForm.GetPositionStoryInList(ANumOfStory: integer): integer;
var
  i, j:integer;
begin
  for i:=0 to Length(ListOfStories)-1 do
  begin
    if ListOfStories[i].Strings[1]=IntToStr(ANumOfStory) then
      j:= i;
  end;
  Result:= j;
end;

function TStoriesForm.CheckAbove(APosition: integer): boolean;
begin
  Result:=(APosition>0);
end;

function TStoriesForm.CheckAboveOrEqualZero(aPosition: integer): Boolean;
begin
  Result:=(APosition>=0);
end;

function TStoriesForm.CheckBelow(APosition: integer): boolean;
begin
  Result:=(APosition<0);
end;

procedure TStoriesForm.AdjustElevation(ARow:integer);
var
  i,sPosition, nPosition:integer;
  vElevation,VHeight:Double;
begin
  for i:= 1 to GetTopIndexInList() do
  begin
   sPosition   := GetPositionStoryInList(i);
   nPosition   := GetPositionStoryInList(i-1);
   vElevation := StrToFloat(ListOfStories[nPosition].Strings[3]);
   VHeight    := StrToFloat(ListOfStories[nPosition].Strings[4]);
   ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',(vElevation+VHeight));
  end;
  for i:= (-1) downto GetBottomIndexInList() do
  begin
   sPosition   := GetPositionStoryInList(i);
   nPosition   := GetPositionStoryInList(i+1);
   vElevation := StrToFloat(ListOfStories[nPosition].Strings[3]);
   VHeight    := StrToFloat(ListOfStories[sPosition].Strings[4]);
   ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',(vElevation-VHeight));
  end;
  LoadStringGrid();
  stgStories.Selection:=TGridRect(Rect(0,ARow,4,ARow));
  stgStories.Col:=2;
  stgStories.Row:=ARow;
end;

procedure TStoriesForm.EditElevation(AEditCase, ARow: integer; AElevation:Double);
var
  sPosition, bPosition, aPosition : integer;
  InferiorLimitElevation,SuperiorLimitElevation : Double;
begin
  sPosition:=GetPositionStoryInList(StrToInt(stgStories.Cells[0,ARow]));
  case AEditCase of
    0: begin
         //Capture de below position:
         bPosition:=GetPositionStoryInList(StrToInt(stgStories.Cells[0,aRow+1]));
         InferiorLimitElevation:=StrToFloatDef(ListOfStories[bPosition].Strings[3],0);
         if AElevation<=InferiorLimitElevation then
         begin
           ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',InferiorLimitElevation);
           ListOfStories[bPosition].Strings[4]:=FormatFloat('#,##0.00',0);
         end else
         begin
           ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',AElevation);
           ListOfStories[bPosition].Strings[4]:=FormatFloat('#,##0.00',(AElevation-InferiorLimitElevation));
         end;
       end;
    1: begin
         // Capture the above position:
         aPosition:=GetPositionStoryInList(StrToInt(stgStories.Cells[0,aRow-1]));
         SuperiorLimitElevation:= StrToFloatDef(ListOfStories[aPosition].Strings[3],0);
         if AElevation>=SuperiorLimitElevation then
         begin
           ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',SuperiorLimitElevation);
           ListOfStories[sPosition].Strings[4]:=FormatFloat('#,##0.00',0);
         end else
         begin
           ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',AElevation);
           ListOfStories[sPosition].Strings[4]:=FormatFloat('#,##0.00',(SuperiorLimitElevation-AElevation));
         end;
       end;
    2: begin
         aPosition:=GetPositionStoryInList(StrToInt(stgStories.Cells[0,aRow-1]));
         SuperiorLimitElevation:= StrToFloatDef(ListOfStories[aPosition].Strings[3],0);
         bPosition:=GetPositionStoryInList(StrToInt(stgStories.Cells[0,aRow+1]));
         InferiorLimitElevation:=StrToFloatDef(ListOfStories[bPosition].Strings[3],0);
         if AElevation<=InferiorLimitElevation then
         begin
           ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',InferiorLimitElevation);
           ListOfStories[sPosition].Strings[4]:=FormatFloat('#,##0.00',(SuperiorLimitElevation-InferiorLimitElevation));
           ListOfStories[bPosition].Strings[4]:=FormatFloat('#,##0.00',0);
         end else
         begin
           if AElevation>=SuperiorLimitElevation then
           begin
             ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',SuperiorLimitElevation);
             ListOfStories[sPosition].Strings[4]:=FormatFloat('#,##0.00',0);
             ListOfStories[bPosition].Strings[4]:=FormatFloat('#,##0.00',(SuperiorLimitElevation-InferiorLimitElevation));
           end else
           begin
             ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',AElevation);
             ListOfStories[sPosition].Strings[4]:=FormatFloat('#,##0.00',(SuperiorLimitElevation-AElevation));
             ListOfStories[bPosition].Strings[4]:=FormatFloat('#,##0.00',(AElevation-InferiorLimitElevation));
           end;
         end;
       end;
  end;
  AdjustElevation(ARow);
end;

procedure TStoriesForm.EditHeight(AEditCase, ARow: integer; AHeight: Double);
var
  sPosition, aPosition : integer;
  sElevation,aElevation : Double;
begin
  sPosition:=GetPositionStoryInList(StrToInt(stgStories.Cells[0,ARow]));
  sElevation:=StrToFloat(ListOfStories[sPosition].Strings[3]);
  case AEditCase of
    0: begin
         ListOfStories[sPosition].Strings[4]:=FormatFloat('#,##0.00',AHeight);
       end;
    1: begin
         aPosition:=GetPositionStoryInList(StrToInt(stgStories.Cells[0,ARow-1]));
         ListOfStories[aPosition].Strings[3]:=FormatFloat('#,##0.00',sElevation+AHeight);
         ListOfStories[sPosition].Strings[4]:=FormatFloat('#,##0.00',AHeight);
       end;
    2,3: begin
         aPosition:=GetPositionStoryInList(StrToInt(stgStories.Cells[0,ARow-1]));
         aElevation:=StrToFloat(ListOfStories[aPosition].Strings[3]);
         ListOfStories[sPosition].Strings[3]:=FormatFloat('#,##0.00',aElevation-AHeight);
         ListOfStories[sPosition].Strings[4]:=FormatFloat('#,##0.00',AHeight);
       end;
  end;
  AdjustElevation(ARow);
end;

{In this procedure, we send as parameter the 04 possible cases of Story insertion:
1: Above Zero and Above the Selected Story;
2: Below Zero and Below the Selected Story;
3: Above Zero and Below the Selected Story;
4: Below Zero and Above the Selected Story:}
procedure TStoriesForm.AddStory(AName: string; InsertionCase:integer);
var
  i, NumOfStory, SelectedIndex,incFactor:integer;
  onTheTop, atTheBottom: Boolean;
begin
  SelectedIndex:= GetNumOfStory(rowSelected);
  for i:=0 to Length(ListOfStories)-1 do
  begin
    onTheTop:=False;
    attheBottom:= False;
    NumOfStory:= GetNumOfStoryInList(i);
    case InsertionCase of
      1: begin
          onTheTop:=(SelectedIndex=GetTopIndexInList());
          incFactor:=1;
          if (NumOfStory>SelectedIndex) and not onTheTop then
          begin
            ListOfStories[i].Strings[1]:= IntToStr(NumOfStory+1);
          end;
         end;
      2: begin
          atTheBottom:=(SelectedIndex=GetBottomIndexInList());
          incFactor:=-1;
          if (NumOfStory<SelectedIndex) and not atTheBottom then
          begin
            ListOfStories[i].Strings[1]:= IntToStr(NumOfStory-1);
          end;
         end;
      3: begin
          incFactor:=0;
          if (NumOfStory>=SelectedIndex) then
          begin
            ListOfStories[i].Strings[1]:= IntToStr(NumOfStory+1);
          end;
         end;
      4: begin
          incFactor:=0;
          if NumOfStory<=SelectedIndex then
          begin
            ListOfStories[i].Strings[1]:= IntToStr(NumOfStory-1);
          end;
         end;
    end;
  end;
  SetLength(ListOfStories,Length(ListOfStories)+1);
  ListOfStories[Length(ListOfStories)-1]:= TStringList.Create;
  ListOfStories[Length(ListOfStories)-1].Add('');
  ListOfStories[Length(ListOfStories)-1].Add(IntToStr(SelectedIndex+incFactor));
  ListOfStories[Length(ListOfStories)-1].Add(AName);
  ListOfStories[Length(ListOfStories)-1].Add(FormatFloat('#,##0.00',0));
  ListOfStories[Length(ListOfStories)-1].Add(FormatFloat('#,##0.00',3));
  ListOfStories[Length(ListOfStories)-1].Add('1');
  AdjustElevation(rowSelected);
end;

procedure TStoriesForm.LoadStringGrid();
var
  firstPosition, checkPosition, n, i: integer;
begin
  n:= Length(ListOfStories);
  firstPosition:= GetTopIndexInList();
  stgStories.RowCount:=1;
  while n>0 do
  begin
    for i:=0 to Length(ListOfStories)-1 do
    begin
      checkPosition:=GetNumOfStoryInList(i);
      if checkPosition = firstPosition then
      begin
        with stgStories do
        begin
          RowCount:= stgStories.RowCount+1;
          Cells[0,(stgStories.RowCount-1)]:= ListOfStories[i].Strings[1];
          Cells[1,(stgStories.RowCount-1)]:= ListOfStories[i].Strings[2];
          Cells[2,(stgStories.RowCount-1)]:= ListOfStories[i].Strings[3];
          Cells[3,(stgStories.RowCount-1)]:= ListOfStories[i].Strings[4];
          Cells[4,(stgStories.RowCount-1)]:= ListOfStories[i].Strings[5];
        end;
        firstPosition:=firstPosition-1;
        n:= n-1;
      end;
    end;
  end;
end;

constructor TStoriesForm.Create(TheOwner: TComponent; AStories: TArchStories);
begin
  Inherited Create(TheOwner);
  ProjectStories:= AStories;
  LoadStories();
  rowSelected:= 1;
end;

end.

