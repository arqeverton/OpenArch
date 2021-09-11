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

unit OpenArchFillTypesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComboEx, ExtCtrls,

  GLScene, GLObjects, GLGeomObjects, GLRenderContextInfo, GLMaterial, StdCtrls,
  GLViewer, GLSLShader, GLTextureSharingShader, GLUserShader,
  GLHiddenLineShader, GLOutlineShader, GLCadencer, GLTexture, GLTextureFormat,
  GLCompositeImage, GLCoordinates, GLCrossPlatform, GLContext,
  GLUtils, GLVectorGeometry, lz_opengl, OpenGLContext,

  // openArch:
  OpenArchGraphicResources;

type

  { TFillTypesForm }

  TFillTypesForm = class(TForm)
    btnAddFillType: TButton;
    btnDelete: TButton;
    btnOK: TButton;
    btnRename: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    ComboBoxEx1: TComboBoxEx;
    GLCamera1: TGLCamera;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLMemoryViewer1: TGLMemoryViewer;
    GLPolygon1: TGLPolygon;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLSLShader1: TGLSLShader;
    GrBoxPatternSize: TGroupBox;
    GrBoxStart: TGroupBox;
    GrBoxGeometric: TGroupBox;
    GrBoxLevel: TGroupBox;
    GrBoxRepetition: TGroupBox;
    ImageList1: TImageList;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    LabeledEdit6: TLabeledEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Button1Click(Sender: TObject);

    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure drawVBOElement();
  private

  public

  end;

var
  FillTypesForm: TFillTypesForm;
  glsl_hatch: TGLProgramHandle;

implementation
uses
  GL, GLext, glu, sdl;

{$R *.frm}

{ TFillTypesForm }

procedure TFillTypesForm.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
const
  vertexShaderFile = 'Source\Shaders\VertexShader.cpp';
  fragmentShaderFile = 'Source\Shaders\BrickFragShader.cpp';
  triangleData: array[0..8] of GLfloat = ( 0.0, 0.0, 0.0,
                                           1.0, 0.0, 0.0,
                                           0.0, 1.0, 0.0);
var
  i: Word;
  VertexArrayID: GLuint;
  triangleVBO: GLuint;

  VertexShaderID: GLuint;
  VertexShaderCode: PGLchar;
  FragmentShaderID: GLuint;
  FragmentShaderCode: PGLchar;
  ShaderCode: TStringList;
  ProgramID: GLuint;
  compilationResult: GLint = GL_FALSE;
  InfoLogLength: GLint;
  ErrorMessageArray: array of GLChar;
  hatch: TArchHatch;
  Params: TArchSDLParameters;
begin

  hatch:= TArchHatch.Create();
  //hatch.VertexShaderProgram.LoadFromFile(vertexShaderFile);
  //hatch.FragmentShaderProgram.LoadFromFile(fragmentShaderFile);
  SetLength(Params,1);
  Params[0]:= TArchSDLParameter.Create('MCposition','(0,0,0)');
  Params[0].SDLQualifiers:= sdlVarying;
  Params[0].SDLType:= sdlVec2;
  hatch.AddParam(Params[0]);
  hatch.VertexShaderProgram.Text:='varying vec2 MCposition;'+LineEnding+
                                  'void main()'+LineEnding+
                                  '{'+LineEnding+
                                  'MCposition = gl_Vertex.xy;'+LineEnding+
                                  'gl_Position = ftransform();'+LineEnding+
                                  '}';
  //hatch.VertexShaderProgram.Add('varying vec2 MCposition;');
  //hatch.VertexShaderProgram.Add('void main()   ');
  //hatch.VertexShaderProgram.Add('{     ');
  //hatch.VertexShaderProgram.Add('MCposition = gl_Vertex.xy;    ');
  //hatch.VertexShaderProgram.Add('gl_Position = ftransform();    ');
  //hatch.VertexShaderProgram.Add('}   ');

  hatch.FragmentShaderProgram.Text:='uniform vec3 BrickColor, MortarColor;  '+LineEnding+
                                    'uniform vec2 BrickSize;  '+LineEnding+
                                    'uniform vec2 BrickPct;  '+LineEnding+
                                    'varying vec2 MCposition;  '+LineEnding+
                                    //'varying float LightIntensity; '+LineEnding+
                                    'const float PI=3.14159265359;   '+LineEnding+
                                    'void main() '+LineEnding+
                                    '{ '+LineEnding+
                                    'vec3 color;'+LineEnding+
                                    'vec2 position, useBrick; '+LineEnding+
                                    'position = MCposition / BrickSize;  '+LineEnding+
                                    'position = fract(position);'+LineEnding+
                                    'float hLines = step(BrickPct.y, position.y); '+LineEnding+
                                    ' color = mix(MortarColor, BrickColor, hLines); '+LineEnding+
                                    'gl_FragColor = vec4(color, 1.0);  '+LineEnding+
                                    '}';

  hatch.Apply();
  drawVBOElement();
  hatch.UnApply();
  {!!!!!!!!!!!!!!!


  //init OpenGL and load extensions
  if Load_GL_VERSION_4_0 = false then
    if Load_GL_VERSION_3_3 = false then
      if Load_GL_VERSION_3_2 = false then
        if Load_GL_VERSION_3_0 = false then
        begin
          ShowMessage(' ERROR: OpenGL 3.0 or higher needed. ');
          HALT;
        end;

  //print out OpenGL vendor, version and shader version
  ShowMessage( 'Vendor: ' + glGetString( GL_VENDOR ) );
  ShowMessage( 'OpenGL Version: ' + glGetString( GL_VERSION ) );
  ShowMessage( 'Shader Version: ' + glGetString( GL_SHADING_LANGUAGE_VERSION ) );


  {------
  //create Vertex Array Object (VAO)
  glGenVertexArrays( 1, @VertexArrayID );
  glBindVertexArray( VertexArrayID );

  //creating Vertex Buffer Object (VBO)
  glGenBuffers( 1, @triangleVBO );
  glBindBuffer( GL_ARRAY_BUFFER, triangleVBO );
  glBufferData( GL_ARRAY_BUFFER, SizeOf( triangleData ), @triangleData, GL_STATIC_DRAW );
  ------}

  //creating shaders
  VertexShaderID := glCreateShader( GL_VERTEX_SHADER );
  FragmentShaderID := glCreateShader( GL_FRAGMENT_SHADER );

  //load shader code and get PChars
  ShaderCode := TStringList.Create;
  ShaderCode.LoadFromFile( VertexShaderFile );
  VertexShaderCode := ShaderCode.GetText;
  if VertexShaderCode = nil then HALT;
  ShaderCode.LoadFromFile( FragmentShaderFile );
  FragmentShaderCode := ShaderCode.GetText;
  if FragmentShaderCode = nil then HALT;
  ShaderCode.Free;

  //compiling and error checking vertex shader
  ShowMessage('Compilando e verificando o Vertex Shader ... ' );
  glShaderSource( VertexShaderID, 1, @VertexShaderCode, nil );
  glCompileShader( VertexShaderID );

  glGetShaderiv( VertexShaderID, GL_COMPILE_STATUS, @compilationResult );
  glGetShaderiv( VertexShaderID, GL_INFO_LOG_LENGTH, @InfoLogLength );
  if compilationResult = GL_FALSE then
  begin
    ShowMessage( 'Falha ao compilar o Vertex Shader ' );
    SetLength( ErrorMessageArray, InfoLogLength+1 );
    glGetShaderInfoLog( VertexShaderID, InfoLogLength, nil, @ErrorMessageArray[0] );
    for i := 0 to InfoLogLength do write( String( ErrorMessageArray[i] ) );
    ;
  end else ShowMessage( 'success' );

  //compiling and error checking fragment shader
  ShowMessage('Compilando e verificando erros do Fragment Shader ... ' );
  glShaderSource( FragmentShaderID, 1, @FragmentShaderCode, nil );
  glCompileShader( FragmentShaderID );

  glGetShaderiv( FragmentShaderID, GL_COMPILE_STATUS, @compilationResult );
  glGetShaderiv( FragmentShaderID, GL_INFO_LOG_LENGTH, @InfoLogLength );
  if compilationResult = GL_FALSE then
  begin
    ShowMessage( 'Falha ao compilar o  fragment shader' );
    SetLength( ErrorMessageArray, InfoLogLength+1 );
    glGetShaderInfoLog( VertexShaderID, InfoLogLength, nil, @ErrorMessageArray[0] );
    for i := 0 to InfoLogLength do write( String( ErrorMessageArray[i] ) );
    ;
  end else ShowMessage( 'success' );

  //creating and linking program
  ShowMessage('Criando e Linkando o Programa... ' );
  ProgramID := glCreateProgram();
  glAttachShader( ProgramID, VertexShaderID );
  glAttachShader( ProgramID, FragmentShaderID );
  glLinkProgram( ProgramID );

  glGetShaderiv( ProgramID, GL_LINK_STATUS, @compilationResult );
  glGetShaderiv( ProgramID, GL_INFO_LOG_LENGTH, @InfoLogLength );
  if compilationResult = GL_FALSE then
  begin
    ShowMessage( 'Falha ' );
    SetLength( ErrorMessageArray, InfoLogLength+1 );
    glGetShaderInfoLog( VertexShaderID, InfoLogLength, nil, @ErrorMessageArray[0] );
    for i := 0 to InfoLogLength do write( String( ErrorMessageArray[i] ) );
    ;
  end else ShowMessage( 'success' );

    glUseProgram( ProgramID );
    glUniform3f(glGetUniformLocation(ProgramID, 'BrickColor'), 1.0, 1.0, 1.0);
    glUniform3f(glGetUniformLocation(ProgramID, 'MortarColor'), 0.0, 0.0, 0.0);
    glUniform2f(glGetUniformLocation(ProgramID, 'BrickSize'), 0.5, 0.5);
    glUniform2f(glGetUniformLocation(ProgramID, 'BrickPct'), 0.2, 0.2);
    glUniform3f(glGetUniformLocation(ProgramID, 'LightPosition'), GLLightSource1.AbsoluteAffinePosition.X,
                                                                  GLLightSource1.AbsoluteAffinePosition.Y,
                                                                  GLLightSource1.AbsoluteAffinePosition.Z);
    drawVBOElement();
   {-------
    glEnableVertexAttribArray( 0 );
    glBindBuffer( GL_ARRAY_BUFFER, triangleVBO );
    glVertexAttribPointer( 0, 3, GL_FLOAT, GL_FALSE, 0, nil );
    glDrawArrays( GL_TRIANGLES, 0, 4 );
    glDisableVertexAttribArray( 0 );
   --------}
  //clean up
  glDetachShader( ProgramID, VertexShaderID );
  glDetachShader( ProgramID, FragmentShaderID );

  glDeleteShader( VertexShaderID );
  glDeleteShader( FragmentShaderID );
  glDeleteProgram( ProgramID );

  StrDispose( VertexShaderCode );
  StrDispose( FragmentShaderCode );
  {---------
  glDeleteBuffers( 1, @triangleVBO );
  glDeleteVertexArrays( 1, @VertexArrayID );
  ------}
  !!!!!!!!!!!!!!!}
end;

procedure TFillTypesForm.drawVBOElement();
const
  vZ: GLuint = 0;
var

  normals:Array of GLfloat =  (0,0,1,
                               0,0,1,
                               0,0,1,
                               0,0,1);
  vertices:Array of GLfloat = (0.0,0.0,0.0,
                               1.0,0.0,0.0,
                               1.0,1.0,0.0,
                               0.0,1.0,0.0);
  colors: Array of GLfloat = (0,0,0,
                              0,0,0,
                              0,0,0,
                              0,0,0);
  indices: array of GLuint = (2,3,0,
                              0,1,2);
  vboID,iboID:GLuint;
  vSize, nSize, cSize, tSize,
  nOffset, cOffset, tOffset: GLsizeiptr;
begin



  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  glVertexPointer(3, GL_FLOAT, 0, @vertices[0]);
  glColorPointer(3, GL_FLOAT, 0, @colors[0]);

  glDrawElements(GL_TRIANGLES, Length(indices), GL_UNSIGNED_INT, @indices[0]);

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);

end;


procedure TFillTypesForm.Button1Click(Sender: TObject);
begin
  GLCamera1.SceneScale:=GLCamera1.SceneScale+0.1;
end;


end.

