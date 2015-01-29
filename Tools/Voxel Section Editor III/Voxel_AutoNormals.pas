unit Voxel_AutoNormals;

interface

uses Voxel, Voxel_Tools, math, Voxel_Engine, math3d, Class3DPointList, SysUtils, Dialogs;

{$define LIMITES}
//{$define RAY_LIMIT}
//{$define DEBUG}

type
// Estruturas do Voxel_Tools and Voxel.
{
   TVector3f = record
      X, Y, Z : single;
   end;

   TApplyNormalsResult = record
      applied,
      confused: Integer;
   end;

   TDistanceArray = array of array of array of TDistanceUnit; //single;
}
// Novas estruturas.
   TVoxelMap = array of array of array of single;
   TFiltroDistanciaUnidade = record
      x,
      y,
      z,
      Distancia : single;
   end;
   TFiltroDistancia = array of array of array of TFiltroDistanciaUnidade;

const
// Essas constantes mapeam o nível de influência do pixel em relação ao
// modelo. As únicas constantes que importam são as não comentadas. As
// outras só estão aí pra fins de referência

   C_FORA_DO_VOLUME = 0;           // não faz parte do modelo
   C_INFLUENCIA_DE_UM_EIXO = 1;    // não faz parte, mas está entre pixels de um eixo
   C_INFLUENCIA_DE_DOIS_EIXOS = 2;  // não faz parte, mas está entre pixels de dois eixos
   C_INFLUENCIA_DE_TRES_EIXOS = 3;  // não faz parte, mas está entre pixels de dois eixos
   C_PARTE_DO_VOLUME = 4;          // parte interna do modelo.
   C_SUPERFICIE = 5;               // superfície do modelo.

   // Constante de pesos, para se usar na hora de detectar a tendência da
   // massa
   PESO_FORA_DO_VOLUME = 0;
   PESO_INFLUENCIA_DE_UM_EIXO = 0.000001;
   PESO_INFLUENCIA_DE_DOIS_EIXOS = 0.0001;
   PESO_INFLUENCIA_DE_TRES_EIXOS = 0.01;
   PESO_PARTE_DO_VOLUME = 0.1;
   PESO_SUPERFICIE = 1;


// Função principal
function AcharNormais(Voxel : TVoxelSection; Alcance : single; TratarDescontinuidades : boolean) : TApplyNormalsResult;

// Funções de mapeamento
procedure InicializaMapaDoVoxel(const Voxel: TVoxelSection; var Mapa : TVoxelMap; Alcance: integer);
procedure FloodFill3D(var Mapa: TVoxelMap);
procedure MesclarMapasBinarios(const Fonte : TVoxelMap; var Destino : TVoxelMap);
procedure MapearInfluencias(const Voxel : TVoxelSection; var Mapa : TVoxelMap; Alcance : integer);
procedure MapearSuperficies(var Mapa : TVoxelMap);
procedure ConverteInfluenciasEmPesos(var Mapa : TVoxelMap);

// Funções de filtro
procedure GerarFiltro(var Filtro : TFiltroDistancia; Alcance : single);
function AplicarFiltroNoMapa(var Voxel : TVoxelSection; const Mapa : TVoxelMap; const Filtro : TFiltroDistancia; Alcance : integer; TratarDescontinuidades : boolean): integer;
procedure AplicarFiltro(var Voxel : TVoxelSection; const Mapa: TVoxelMap; const Filtro : TFiltroDistancia; var V : TVoxelUnpacked; Alcance,_x,_y,_z : integer; TratarDescontinuidades : boolean = false);

// 1.38: Funções de detecção de superfícies.
procedure DetectarSuperficieContinua(var MapaDaSuperficie: TBooleanMap; const Mapa : TVoxelMap; const Filtro : TFiltroDistancia; x,y,z : integer; const PontoMin,PontoMax : TVector3i; var LimiteMin,LimiteMax : TVector3i);
procedure DetectarSuperficieEsferica(var MapaDaSuperficie: TBooleanMap; const Mapa : TVoxelMap; const Filtro : TFiltroDistancia; const PontoMin,PontoMax: TVector3i; var LimiteMin,LimiteMax : TVector3i);


// Plano Tangente
procedure AcharPlanoTangenteEmXY(const Mapa : TBooleanMap; const Filtro : TFiltroDistancia; Alcance : integer; Meio: TVector3i; var PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste : TVector3f; const LimiteMin, LimiteMax : TVector3i);
procedure AcharPlanoTangenteEmYZ(const Mapa : TBooleanMap; const Filtro : TFiltroDistancia; Alcance : integer; Meio: TVector3i; var PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste : TVector3f; const LimiteMin, LimiteMax : TVector3i);
procedure AcharPlanoTangenteEmXZ(const Mapa : TBooleanMap; const Filtro : TFiltroDistancia; Alcance : integer; Meio: TVector3i; var PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste : TVector3f; const LimiteMin, LimiteMax : TVector3i);


// Outras funções
function PontoValido (const x,y,z,maxx,maxy,maxz : integer) : boolean;
function PegarValorDoPonto(const Mapa : TVoxelMap; var Ultimo : TVector3i; const Ponto : TVector3f; var EstaNoVazio : boolean): single;
procedure AdicionaNaListaSuperficie(const Mapa: TVoxelMap; const Filtro : TFiltroDistancia; x,y,z,xdir,ydir,zdir,XFiltro,YFiltro,ZFiltro: integer; var Lista,Direcao : C3DPointList; var LimiteMin,LimiteMax : TVector3i; var MapaDeVisitas,MapaDeSuperficie : TBooleanMap);


implementation

// 1.37: Novo Auto-Normalizador baseado em planos tangentes
// Essa é a função principal da normalização.
function AcharNormais(Voxel : TVoxelSection; Alcance : single; TratarDescontinuidades : boolean) : TApplyNormalsResult;
var
   MapaDoVoxel : TVoxelMap;
   Filtro : TFiltroDistancia;
   IntAlcance : integer;
begin
   // Reseta as variáveis mais básicas.
   Result.applied := 0;
   IntAlcance := Trunc(Alcance);
   // Estratégia: Primeiro mapeamos o voxel, preparamos o filtro e depois
   // aplicamos o filtro no mapa.

   // ----------------------------------------------------
   // Parte 1: Mapeando as superfícies, parte interna e influências do
   // modelo.
   // ----------------------------------------------------
   InicializaMapaDoVoxel(Voxel,MapaDoVoxel,IntAlcance);
   MapearInfluencias(Voxel,MapaDoVoxel,IntAlcance);
   MapearSuperficies(MapaDoVoxel);
   ConverteInfluenciasEmPesos(MapaDoVoxel);

   // ----------------------------------------------------
   // Parte 2: Preparando o filtro.
   // ----------------------------------------------------
   GerarFiltro(Filtro,Alcance);

   // ----------------------------------------------------
   // Parte 3: Aplicando o filtro no mapa e achando as normais
   // ----------------------------------------------------
   Result.applied := AplicarFiltroNoMapa(Voxel,MapaDoVoxel,Filtro,IntAlcance,TratarDescontinuidades);

   // ----------------------------------------------------
   // Parte 4: Libera memória.
   // ----------------------------------------------------
   Finalize(Filtro);
   Finalize(MapaDoVoxel);
end;

///////////////////////////////////////////////////////////////////////
///////////////// Funções de Mapeamento ///////////////////////////////
///////////////////////////////////////////////////////////////////////
//////////////////////////
////////////////
///////

procedure InicializaMapaDoVoxel(const Voxel: TVoxelSection; var Mapa : TVoxelMap; Alcance: integer);
var
   DuploAlcance : integer;
   x,y,z : integer;
   MapaPreenchido : TVoxelMap;
   V : TVoxelUnpacked;
begin
   DuploAlcance := 2 * Alcance;
   // Reserva memória para o mapa. Memória extra evita validação dos pontos
   // à serem avaliados se são ou não parte do modelo.
   SetLength(Mapa, Voxel.Tailer.XSize + DuploAlcance, Voxel.Tailer.YSize + DuploAlcance, Voxel.Tailer.ZSize + DuploAlcance);
   // Mapa preenchido é temporário e vai ser utilizado pra determinar o
   // volume do sólido
   SetLength(MapaPreenchido, Voxel.Tailer.XSize + DuploAlcance, Voxel.Tailer.YSize + DuploAlcance, Voxel.Tailer.ZSize + DuploAlcance);

   // Inicializa mapa
   for x := Low(Mapa) to High(Mapa) do
      for y := Low(Mapa) to High(Mapa[x]) do
         for z := Low(Mapa) to High(Mapa[x,y]) do
         begin
            // Get voxel data.
            if Voxel.GetVoxelSafe(x-Alcance,y-Alcance,z-Alcance,v) then
            begin
               // Check if it's used.
               if v.Used then
               begin
                  Mapa[x,y,z] := 1;
                  MapaPreenchido[x,y,z] := 0;
               end
               else
               begin
                  Mapa[x,y,z] := 0;
                  MapaPreenchido[x,y,z] := 1;
               end
            end
            else
            begin
               Mapa[x,y,z] := 0;
               MapaPreenchido[x,y,z] := 1;
            end;
         end;

   // Vamos preencher tudo que está fora pra descobrir quem está dentro
   FloodFill3D(MapaPreenchido);
   // Joga o que está dentro nas bordas.
   MesclarMapasBinarios(MapaPreenchido,Mapa);
   Finalize(MapaPreenchido);
end;

// 3D Flood Fill para encontrar o volume interno do modelo
procedure FloodFill3D(var Mapa: TVoxelMap);
var
   Lista : C3DPointList; // Veja Class3DPointList.pas;
   x,y,z : integer;
   maxx,maxy,maxz : integer;
begin
   // Pega o máximo valor pra cada eixo.
   maxx := High(Mapa);
   maxy := High(Mapa[0]);
   maxz := High(Mapa[0,0]);

   // Começa em (0,0,0);
   Lista := C3DPointList.Create;
   Lista.Add(0,0,0);
   Mapa[0,0,0] := 0;
   // Vai preencher enquanto houver elementos na lista.
   while Lista.GetPosition(x,y,z) do
   begin
      // Confere e adiciona os vizinhos (6 faces)
      if PontoValido(x-1,y,z,maxx,maxy,maxz) then
         if Mapa[x-1,y,z] = 1 then
         begin
            Mapa[x-1,y,z] := 0;
            Lista.Add(x-1,y,z);
         end;
      if PontoValido(x+1,y,z,maxx,maxy,maxz) then
         if Mapa[x+1,y,z] = 1 then
         begin
            Mapa[x+1,y,z] := 0;
            Lista.Add(x+1,y,z);
         end;
      if PontoValido(x,y-1,z,maxx,maxy,maxz) then
         if Mapa[x,y-1,z] = 1 then
         begin
            Mapa[x,y-1,z] := 0;
            Lista.Add(x,y-1,z);
         end;
      if PontoValido(x,y+1,z,maxx,maxy,maxz) then
         if Mapa[x,y+1,z] = 1 then
         begin
            Mapa[x,y+1,z] := 0;
            Lista.Add(x,y+1,z);
         end;
      if PontoValido(x,y,z-1,maxx,maxy,maxz) then
         if Mapa[x,y,z-1] = 1 then
         begin
            Mapa[x,y,z-1] := 0;
            Lista.Add(x,y,z-1);
         end;
      if PontoValido(x,y,z+1,maxx,maxy,maxz) then
         if Mapa[x,y,z+1] = 1 then
         begin
            Mapa[x,y,z+1] := 0;
            Lista.Add(x,y,z+1);
         end;
      Lista.GoToNextElement;
   end;
   Lista.Free;
end;

procedure MesclarMapasBinarios(const Fonte : TVoxelMap; var Destino : TVoxelMap);
var
   x,y,z : integer;
begin
   // Copia todo '1' da fonte no destino.
   for x := 0 to High(Fonte) do
      for y := 0 to High(Fonte[x]) do
         for z := 0 to High(Fonte[x,y]) do
         begin
            if Fonte[x,y,z] = 1 then
               Destino[x,y,z] := 1;
         end;
end;



procedure MapearInfluencias(const Voxel : TVoxelSection; var Mapa : TVoxelMap; Alcance : integer);
var
   x,y,z : integer;
   PontoInicial,PontoFinal : integer;
   V : TVoxelUnpacked;
begin
   // Varre o modelo na direção Z
   for x := Low(Mapa) to High(Mapa) do
      for y := Low(Mapa) to High(Mapa[x]) do
      begin
         // Pega o Ponto Inicial
         z := Low(Mapa[x,y]);
         PontoInicial := -1;
         while (z <= High(Mapa[x,y])) and (PontoInicial = -1) do
         begin
            if Voxel.GetVoxelSafe(x-Alcance,y-Alcance,z-Alcance,v) then
            begin
               if v.Used then
               begin
                  PontoInicial := z;
               end;
            end;
            inc(z);
         end;
         // Pega o Ponto Final, se existir pizel usado o eixo
         if PontoInicial <> -1 then
         begin
            z := High(Mapa[x,y]);
            PontoFinal := -1;
            while (z >= Low(Mapa[x,y])) and (PontoFinal = -1) do
            begin
               if Voxel.GetVoxelSafe(x-Alcance,y-Alcance,z-Alcance,v) then
               begin
                  if v.Used then
                  begin
                     PontoFinal := z;
                  end;
               end;
               dec(z);
            end;
            // Agora preenchemos tudo entre o Ponto Inicial e o Ponto Final
            z := PontoInicial;
            while z <= PontoFinal do
            begin
               Mapa[x,y,z] := Mapa[x,y,z] + 1;
               inc(z);
            end;
         end;
      end;

   // Varre o modelo na direção X
   for y := Low(Mapa[0]) to High(Mapa[0]) do
      for z := Low(Mapa[0,y]) to High(Mapa[0,y]) do
      begin
         // Pega o Ponto Inicial
         x := Low(Mapa);
         PontoInicial := -1;
         while (x <= High(Mapa)) and (PontoInicial = -1) do
         begin
            if Voxel.GetVoxelSafe(x-Alcance,y-Alcance,z-Alcance,v) then
            begin
               if v.Used then
               begin
                  PontoInicial := x;
               end;
            end;
            inc(x);
         end;
         // Pega o Ponto Final, se existir pizel usado o eixo
         if PontoInicial <> -1 then
         begin
            x := High(Mapa);
            PontoFinal := -1;
            while (x >= Low(Mapa)) and (PontoFinal = -1) do
            begin
               if Voxel.GetVoxelSafe(x-Alcance,y-Alcance,z-Alcance,v) then
               begin
                  if v.Used then
                  begin
                     PontoFinal := x;
                  end;
               end;
               dec(x);
            end;
            // Agora preenchemos tudo entre o Ponto Inicial e o Ponto Final
            x := PontoInicial;
            while x <= PontoFinal do
            begin
               Mapa[x,y,z] := Mapa[x,y,z] + 1;
               inc(x);
            end;
         end;
      end;

   // Varre o modelo na direção Y
   for x := Low(Mapa) to High(Mapa) do
      for z := Low(Mapa[x,0]) to High(Mapa[x,0]) do
      begin
         // Pega o Ponto Inicial
         y := Low(Mapa[x]);
         PontoInicial := -1;
         while (y <= High(Mapa[x])) and (PontoInicial = -1) do
         begin
            if Voxel.GetVoxelSafe(x-Alcance,y-Alcance,z-Alcance,v) then
            begin
               if v.Used then
               begin
                  PontoInicial := y;
               end;
            end;
            inc(y);
         end;
         // Pega o Ponto Final, se existir pizel usado o eixo
         if PontoInicial <> -1 then
         begin
            y := High(Mapa[x]);
            PontoFinal := -1;
            while (y >= Low(Mapa[x])) and (PontoFinal = -1) do
            begin
               if Voxel.GetVoxelSafe(x-Alcance,y-Alcance,z-Alcance,v) then
               begin
                  if v.Used then
                  begin
                     PontoFinal := y;
                  end;
               end;
               dec(y);
            end;
            // Agora preenchemos tudo entre o Ponto Inicial e o Ponto Final
            y := PontoInicial;
            while y <= PontoFinal do
            begin
               Mapa[x,y,z] := Mapa[x,y,z] + 1;
               inc(y);
            end;
         end;
      end;
end;

// Essa função requer que as influências já estejam mapeadas.
procedure MapearSuperficies(var Mapa : TVoxelMap);
var
   x,y,z : integer;
   MaxX,MaxY,MaxZ : integer;
   DentroDoVolume : boolean;
begin
   MaxX := High(Mapa);
   MaxY := High(Mapa[0]);
   MaxZ := High(Mapa[0,0]);
   // Varre o modelo na direção Z
   for x := Low(Mapa) to High(Mapa) do
      for y := Low(Mapa) to High(Mapa[x]) do
      begin
         z := Low(Mapa[x,y]);
         // A varredura da linha sempre começa fora do volume
         DentroDoVolume := false;
         while z <= High(Mapa[x,y]) do
         begin
            if DentroDoVolume then
            begin
               // Se um ponto (X,Y,Z) não está no volume, então seu
               // anterior é superfície
               while (z <= High(Mapa[x,y])) and DentroDoVolume do
               begin
                  if Mapa[x,y,z] < C_PARTE_DO_VOLUME then
                  begin
                     Mapa[x,y,z-1] := C_SUPERFICIE;
                     // e o que vem antes dele pode ser parte da superf[icie
                     // dependendo dos oito vizinhos (x = x-1,..,x+1; y = y-1,...,y+1 e x <> y)
                     if PontoValido(x-1,y-1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y-1,z-2] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z-2] := C_SUPERFICIE;
                     end
                     else if PontoValido(x,y-1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x,y-1,z-2] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z-2] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y-1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y-1,z-2] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z-2] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-1,y,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y,z-2] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z-2] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y,z-2] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z-2] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-1,y+1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y+1,z-2] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z-2] := C_SUPERFICIE;
                     end
                     else if PontoValido(x,y+1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x,y+1,z-2] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z-2] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y+1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y+1,z-2] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z-2] := C_SUPERFICIE;
                     end;
                     // e sai do volume
                     DentroDoVolume := false;
                  end;
                  inc(z);
               end;
            end
            else // não está dentro do volume..
            begin
               // Se um ponto (X,Y,Z) está no volume, então ele é superfície
               while (z <= High(Mapa[x,y])) and (not DentroDoVolume) do
               begin
                  if Mapa[x,y,z] >= C_PARTE_DO_VOLUME then
                  begin
                     Mapa[x,y,z] := C_SUPERFICIE;
                     // e o que vem depois dele pode ser parte da superf[icie
                     // dependendo dos oito vizinhos (x = x-1,..,x+1; y = y-1,...,y+1 e x <> y)
                     if PontoValido(x-1,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y-1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z+1] := C_SUPERFICIE;
                     end
                     else if PontoValido(x,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x,y-1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z+1] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y-1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z+1] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-1,y,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z+1] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z+1] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y+1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z+1] := C_SUPERFICIE;
                     end
                     else if PontoValido(x,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x,y+1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z+1] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y+1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y,z+1] := C_SUPERFICIE;
                     end;
                     // e entra no volume
                     DentroDoVolume := true;
                  end;
                  inc(z);
               end;
            end;
         end;
      end;

   // Varre o modelo na direção X
   for y := Low(Mapa[0]) to High(Mapa[0]) do
      for z := Low(Mapa[0,y]) to High(Mapa[0,y]) do
      begin
         x := Low(Mapa);
         // A varredura da linha sempre começa fora do volume
         DentroDoVolume := false;
         while x <= High(Mapa) do
         begin
            if DentroDoVolume then
            begin
               // Se um ponto (X,Y,Z) não está no volume, então seu
               // anterior é superfície
               while (x <= High(Mapa)) and DentroDoVolume do
               begin
                  if Mapa[x,y,z] < C_PARTE_DO_VOLUME then
                  begin
                     Mapa[x-1,y,z] := C_SUPERFICIE;
                     // e o que vem antes dele pode ser parte da superf[icie
                     // dependendo dos oito vizinhos (z = z-1,..,z+1; y = y-1,...,y+1 e z <> y)
                     if PontoValido(x-2,y-1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-2,y-1,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x-2,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-2,y-1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-2,y-1,z] < C_PARTE_DO_VOLUME then
                           Mapa[x-2,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-2,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-2,y-1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x-2,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-2,y,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-2,y,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x-2,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-2,y,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-2,y,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x-2,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-2,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-2,y+1,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x-2,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-2,y+1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-2,y+1,z] < C_PARTE_DO_VOLUME then
                           Mapa[x-2,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-2,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-2,y+1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x-2,y,z] := C_SUPERFICIE;
                     end;
                     // e sai do volume
                     DentroDoVolume := false;
                  end;
                  inc(x);
               end;
            end
            else // não está dentro do volume..
            begin
               // Se um ponto (X,Y,Z) está no volume, então ele é superfície
               while (x <= High(Mapa)) and (not DentroDoVolume) do
               begin
                  if Mapa[x,y,z] >= C_PARTE_DO_VOLUME then
                  begin
                     Mapa[x,y,z] := C_SUPERFICIE;
                     // e o que vem depois dele pode ser parte da superf[icie
                     // dependendo dos oito vizinhos (z = z-1,..,z+1; y = y-1,...,y+1 e z <> y)
                     if PontoValido(x+1,y-1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y-1,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x+1,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y-1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y-1,z] < C_PARTE_DO_VOLUME then
                           Mapa[x+1,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y-1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x+1,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x+1,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x+1,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y+1,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x+1,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y+1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y+1,z] < C_PARTE_DO_VOLUME then
                           Mapa[x+1,y,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y+1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x+1,y,z] := C_SUPERFICIE;
                     end;
                     // e entra no volume
                     DentroDoVolume := true;
                  end;
                  inc(x);
               end;
            end;
         end;
      end;

   // Varre o modelo na direção Y
   for x := Low(Mapa) to High(Mapa) do
      for z := Low(Mapa[x,0]) to High(Mapa[x,0]) do
      begin
         y := Low(Mapa[x]);
         // A varredura da linha sempre começa fora do volume
         DentroDoVolume := false;
         while y <= High(Mapa[x]) do
         begin
            if DentroDoVolume then
            begin
               // Se um ponto (X,Y,Z) não está no volume, então seu
               // anterior é superfície
               while (y <= High(Mapa[x])) and DentroDoVolume do
               begin
                  if Mapa[x,y,z] < C_PARTE_DO_VOLUME then
                  begin
                     Mapa[x,y-1,z] := C_SUPERFICIE;
                     // e o que vem antes dele pode ser parte da superf[icie
                     // dependendo dos oito vizinhos (x = x-1,..,x+1; z = z-1,...,z+1 e x <> z)
                     if PontoValido(x-1,y-2,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y-2,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y-2,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x,y-2,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x,y-2,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y-2,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y-2,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y-2,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y-2,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-1,y-2,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y-2,z] < C_PARTE_DO_VOLUME then
                           Mapa[x,y-2,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y-2,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y-2,z] < C_PARTE_DO_VOLUME then
                           Mapa[x,y-2,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-1,y-2,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y-2,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y-2,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x,y-2,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x,y-2,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y-2,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y-2,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y-2,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y-2,z] := C_SUPERFICIE;
                     end;
                     // e sai do volume
                     DentroDoVolume := false;
                  end;
                  inc(y);
               end;
            end
            else // não está dentro do volume..
            begin
               // Se um ponto (X,Y,Z) está no volume, então ele é superfície
               while (y <= High(Mapa[x])) and (not DentroDoVolume) do
               begin
                  if Mapa[x,y,z] >= C_PARTE_DO_VOLUME then
                  begin
                     Mapa[x,y,z] := C_SUPERFICIE;
                     // e o que vem depois dele pode ser parte da superf[icie
                     // dependendo dos oito vizinhos (x = x-1,..,x+1; z = z-1,...,z+1 e x <> z)
                     if PontoValido(x-1,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y+1,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y+1,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x,y+1,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y+1,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y+1,z-1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y+1,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-1,y+1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y+1,z] < C_PARTE_DO_VOLUME then
                           Mapa[x,y+1,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y+1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y+1,z] < C_PARTE_DO_VOLUME then
                           Mapa[x,y+1,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x-1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x-1,y+1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y+1,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x,y+1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y+1,z] := C_SUPERFICIE;
                     end
                     else if PontoValido(x+1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Mapa[x+1,y+1,z+1] < C_PARTE_DO_VOLUME then
                           Mapa[x,y+1,z] := C_SUPERFICIE;
                     end;
                     // e entra no volume
                     DentroDoVolume := true;
                  end;
                  inc(y);
               end;
            end;
         end;
      end;
end;

procedure ConverteInfluenciasEmPesos(var Mapa : TVoxelMap);
var
   x,y,z : integer;
   Peso : array [0..5] of single;
begin
   Peso[C_FORA_DO_VOLUME] := PESO_FORA_DO_VOLUME;
   Peso[C_INFLUENCIA_DE_UM_EIXO] := PESO_INFLUENCIA_DE_UM_EIXO;
   Peso[C_INFLUENCIA_DE_DOIS_EIXOS] := PESO_INFLUENCIA_DE_DOIS_EIXOS;
   Peso[C_INFLUENCIA_DE_TRES_EIXOS] := PESO_INFLUENCIA_DE_TRES_EIXOS;
   Peso[C_PARTE_DO_VOLUME] := PESO_PARTE_DO_VOLUME;
   Peso[C_SUPERFICIE] := PESO_SUPERFICIE;

   for x := Low(Mapa) to High(Mapa) do
      for y := Low(Mapa[x]) to High(Mapa[x]) do
         for z := Low(Mapa[x,y]) to High(Mapa[x,y]) do
         begin
            Mapa[x,y,z] := Peso[Round(Mapa[x,y,z])];
         end;
end;

///////////////////////////////////////////////////////////////////////
///////////////// Funções de Filtro ///////////////////////////////////
///////////////////////////////////////////////////////////////////////
//////////////////////////
////////////////
///////

procedure GerarFiltro(var Filtro : TFiltroDistancia; Alcance : single);
var
   x,y,z : integer;
   Tamanho,Meio : integer;
   Distancia,DistanciaAoCubo : single;
begin
   // 1.36 Setup distance array
   Meio := Trunc(Alcance);
   Tamanho := (2*Meio)+1;
   SetLength(Filtro,Tamanho,Tamanho,Tamanho);
   Filtro[Meio,Meio,Meio].X := 0;
   Filtro[Meio,Meio,Meio].Y := 0;
   Filtro[Meio,Meio,Meio].Z := 0;
   for x := Low(Filtro) to High(Filtro) do
   begin
      for y := Low(Filtro[x]) to High(Filtro[x]) do
      begin
         for z := Low(Filtro[x,y]) to High(Filtro[x,y]) do
         begin
            Distancia := sqrt(((x - Meio) * (x - Meio)) + ((y - Meio) * (y - Meio)) + ((z - Meio) * (z - Meio)));
            if (Distancia > 0) and (Distancia <= Alcance) then
            begin
               DistanciaAoCubo := Power(Distancia,3);
               if Meio <> x then
                  Filtro[x,y,z].X :=  (Meio - x) / DistanciaAoCubo
               else
                  Filtro[x,y,z].X := 0;

               if Meio <> y then
                  Filtro[x,y,z].Y :=  (Meio - y) / DistanciaAoCubo
               else
                  Filtro[x,y,z].Y := 0;

               if Meio <> z then
                  Filtro[x,y,z].Z :=  (Meio - z) / DistanciaAoCubo
               else
                  Filtro[x,y,z].Z := 0;
            end
            else
            begin
               Filtro[x,y,z].X := 0;
               Filtro[x,y,z].Y := 0;
               Filtro[x,y,z].Z := 0;
            end;
         end;
      end;
   end;
end;

function AplicarFiltroNoMapa(var Voxel : TVoxelSection; const Mapa : TVoxelMap; const Filtro : TFiltroDistancia; Alcance : integer; TratarDescontinuidades : boolean): integer;
var
   x,y,z : integer;
   v : TVoxelUnpacked;
begin
   Result := 0;
   for x := 0 to Voxel.Tailer.XSize-1 do
      for y := 0 to Voxel.Tailer.YSize-1 do
         for z := 0 to Voxel.Tailer.ZSize-1 do
         begin
            // Get voxel data and calculate it (added +1 to
            // each, since binary map has propositally a
            // border to avoid bound checking).
            Voxel.GetVoxel(x,y,z,v);
            if v.Used then
            begin
               AplicarFiltro(Voxel,Mapa,Filtro,v,Alcance,x,y,z,TratarDescontinuidades);
               inc(Result);
            end;
         end;
end;

procedure AplicarFiltro(var Voxel : TVoxelSection; const Mapa: TVoxelMap; const Filtro : TFiltroDistancia; var V : TVoxelUnpacked; Alcance,_x,_y,_z : integer; TratarDescontinuidades : boolean = false);
const
   C_TAMANHO_RAYCASTING = 12;
var
   VetorNormal : TVector3f;
   x,y,z : integer;
   xx,yy,zz : integer;
   PontoMin,PontoMax,LimiteMin,LimiteMax,PseudoCentro: TVector3i;
   PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste : TVector3f;
   Direcao : single;
   Contador : integer;
{$ifdef RAY_LIMIT}
   ValorFrente,ValorOposto : real;
{$endif}
   PararRaioDaFrente,PararRaioOposto : boolean;
   Posicao,PosicaoOposta,Centro : TVector3f;
   MapaDaSuperficie : TBooleanMap;
   UltimoVisitado,UltimoOpostoVisitado : TVector3i;
begin
   // Esse é o ponto do Mapa equivalente ao ponto do voxel a ser avaliado.
   x := Alcance + _x;
   y := Alcance + _y;
   z := Alcance + _z;

   // Temos os limites máximos e mínimos da vizinhança que será verificada no
   // processo
   PontoMin.X := _x;
   PontoMax.X := x + Alcance;
   PontoMin.Y := _y;
   PontoMax.Y := y + Alcance;
   PontoMin.Z := _z;
   PontoMax.Z := z + Alcance;

   // 1.38: LimiteMin e LimiteMax são a 'bounding box' do plano tangente a ser
   // avaliado.
{$ifdef LIMITES}
   LimiteMin := SetVectori(Alcance,Alcance,Alcance);
   LimiteMax := SetVectori(Alcance,Alcance,Alcance);
{$else}
   LimiteMin := SetVectori(0,0,0);
   LimiteMax := SetVectori(High(Filtro),High(Filtro[0]),High(Filtro[0,0]));
{$endif}

   // 1.38: Agora vamos conferir os voxels que farão parte da superfície
   // analisada.
   SetLength(MapaDaSuperficie,High(Filtro)+1,High(Filtro[0])+1,High(Filtro[0,0])+1);

   if TratarDescontinuidades then
   begin
      DetectarSuperficieContinua(MapaDaSuperficie,Mapa,Filtro,x,y,z,PontoMin,PontoMax,LimiteMin,LimiteMax);
   end
   else
   begin
      // Senão, adicionaremos os pontos que façam parte da superfície do modelo.
      DetectarSuperficieEsferica(MapaDaSuperficie,Mapa,Filtro,PontoMin,PontoMax,LimiteMin,LimiteMax);
   end;

{$ifdef LIMITES}
   // Isso calcula o que será considerado o centro do plano tangente.
   PseudoCentro.X := (LimiteMin.X + LimiteMax.X) div 2;
   PseudoCentro.Y := (LimiteMin.Y + LimiteMax.Y) div 2;
   PseudoCentro.Z := (LimiteMin.Z + LimiteMax.Z) div 2;
{$else}
   PseudoCentro.X := Alcance;
   PseudoCentro.Y := Alcance;
   PseudoCentro.Z := Alcance;
{$endif}
   // Resetamos os pontos do plano
   PontoSudoeste := SetVector(0,0,0);
   PontoNordeste := SetVector(0,0,0);
   // Vetor Normal
   VetorNormal := SetVector(0,0,0);

   // Para encontrar o plano tangente, primeiro iremos ver qual é a
   // provável tendência desse plano, para evitar arestas pequenas demais
   // que distorceriam o resultado final.
   for xx := Low(MapaDaSuperficie) to High(MapaDaSuperficie) do
      for yy := Low(MapaDaSuperficie[xx]) to High(MapaDaSuperficie[xx]) do
         for zz := Low(MapaDaSuperficie[xx,yy]) to High(MapaDaSuperficie[xx,yy]) do
         begin
            if MapaDaSuperficie[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               if (Filtro[xx,yy,zz].X >= 0) then
                  PontoNordeste.X := PontoNordeste.X + Filtro[xx,yy,zz].X
               else
                  PontoSudoeste.X := PontoSudoeste.X + Filtro[xx,yy,zz].X;

               if (Filtro[xx,yy,zz].Y >= 0) then
                  PontoNordeste.Y := PontoNordeste.Y + Filtro[xx,yy,zz].Y
               else
                  PontoSudoeste.Y := PontoSudoeste.Y + Filtro[xx,yy,zz].Y;

               if (Filtro[xx,yy,zz].Z >= 0) then
                  PontoNordeste.Z := PontoNordeste.Z + Filtro[xx,yy,zz].Z
               else
                  PontoSudoeste.Z := PontoSudoeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Com a tendência acima, vamos escolher os dois maiores eixos
   if (PontoNordeste.X - PontoSudoeste.X) > (PontoNordeste.Y - PontoSudoeste.Y) then
   begin
      if (PontoNordeste.Y - PontoSudoeste.Y) > (PontoNordeste.Z - PontoSudoeste.Z) then
      begin
         AcharPlanoTangenteEmXY(MapaDaSuperficie,Filtro,Alcance,PseudoCentro,PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste,LimiteMin,LimiteMax);
      end
      else
      begin
         AcharPlanoTangenteEmXZ(MapaDaSuperficie,Filtro,Alcance,PseudoCentro,PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste,LimiteMin,LimiteMax);
      end;
   end
   else if (PontoNordeste.X - PontoSudoeste.X) > (PontoNordeste.Z - PontoSudoeste.Z) then
   begin
      AcharPlanoTangenteEmXY(MapaDaSuperficie,Filtro,Alcance,PseudoCentro,PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste,LimiteMin,LimiteMax);
   end
   else
   begin
      AcharPlanoTangenteEmYZ(MapaDaSuperficie,Filtro,Alcance,PseudoCentro,PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste,LimiteMin,LimiteMax);
   end;

   // Agora vamos achar uma das direções do plano. A outra vai ser o
   // negativo dessa.
   VetorNormal.X := ((PontoNordeste.Y - PontoSudeste.Y) * (PontoSudoeste.Z - PontoSudeste.Z)) - ((PontoSudoeste.Y - PontoSudeste.Y) * (PontoNordeste.Z - PontoSudeste.Z));
   VetorNormal.Y := ((PontoNordeste.Z - PontoSudeste.Z) * (PontoSudoeste.X - PontoSudeste.X)) - ((PontoSudoeste.Z - PontoSudeste.Z) * (PontoNordeste.X - PontoSudeste.X));
   VetorNormal.Z := ((PontoNordeste.X - PontoSudeste.X) * (PontoSudoeste.Y - PontoSudeste.Y)) - ((PontoSudoeste.X - PontoSudeste.X) * (PontoNordeste.Y - PontoSudeste.Y));

   if (VetorNormal.X = 0) and (VetorNormal.Y = 0) and (VetorNormal.Z = 0) then
   begin
      VetorNormal.X := ((PontoNoroeste.Y - PontoNordeste.Y) * (PontoSudeste.Z - PontoNordeste.Z)) - ((PontoSudeste.Y - PontoNordeste.Y) * (PontoNoroeste.Z - PontoNordeste.Z));
      VetorNormal.Y := ((PontoNoroeste.Z - PontoNordeste.Z) * (PontoSudeste.X - PontoNordeste.X)) - ((PontoSudeste.Z - PontoNordeste.Z) * (PontoNoroeste.X - PontoNordeste.X));
      VetorNormal.Z := ((PontoNoroeste.X - PontoNordeste.X) * (PontoSudeste.Y - PontoNordeste.Y)) - ((PontoSudeste.X - PontoNordeste.X) * (PontoNoroeste.Y - PontoNordeste.Y));
   end;
   if (VetorNormal.X = 0) and (VetorNormal.Y = 0) and (VetorNormal.Z = 0) then
   begin
      VetorNormal.X := ((PontoSudoeste.Y - PontoNoroeste.Y) * (PontoNordeste.Z - PontoNoroeste.Z)) - ((PontoNordeste.Y - PontoNoroeste.Y) * (PontoSudoeste.Z - PontoNoroeste.Z));
      VetorNormal.Y := ((PontoSudoeste.Z - PontoNoroeste.Z) * (PontoNordeste.X - PontoNoroeste.X)) - ((PontoNordeste.Z - PontoNoroeste.Z) * (PontoSudoeste.X - PontoNoroeste.X));
      VetorNormal.Z := ((PontoSudoeste.X - PontoNoroeste.X) * (PontoNordeste.Y - PontoNoroeste.Y)) - ((PontoNordeste.X - PontoNoroeste.X) * (PontoSudoeste.Y - PontoNoroeste.Y));
   end;
   if (VetorNormal.X = 0) and (VetorNormal.Y = 0) and (VetorNormal.Z = 0) then
   begin
      VetorNormal.X := ((PontoSudeste.Y - PontoSudoeste.Y) * (PontoNoroeste.Z - PontoSudoeste.Z)) - ((PontoNoroeste.Y - PontoSudoeste.Y) * (PontoSudeste.Z - PontoSudoeste.Z));
      VetorNormal.Y := ((PontoSudeste.Z - PontoSudoeste.Z) * (PontoNoroeste.X - PontoSudoeste.X)) - ((PontoNoroeste.Z - PontoSudoeste.Z) * (PontoSudeste.X - PontoSudoeste.X));
      VetorNormal.Z := ((PontoSudeste.X - PontoSudoeste.X) * (PontoNoroeste.Y - PontoSudoeste.Y)) - ((PontoNoroeste.X - PontoSudoeste.X) * (PontoSudeste.Y - PontoSudoeste.Y));
{$ifdef DEBUG}
      if (VetorNormal.X = 0) and (VetorNormal.Y = 0) and (VetorNormal.Z = 0) then
//      ShowMessage('(' + FloatToStr(VetorNormal.X) + ',' + FloatToStr(VetorNormal.Y) + ',' + FloatToStr(VetorNormal.Z) + ')');
         ShowMessage('(0,0,0) with (' + FloatToStr(PontoSudoeste.X) + ',' + FloatToStr(PontoSudoeste.Y) + ',' + FloatToStr(PontoSudoeste.Z) + ') - (' + FloatToStr(PontoSudeste.X) + ',' + FloatToStr(PontoSudeste.Y) + ',' + FloatToStr(PontoSudeste.Z) + ') - (' + FloatToStr(PontoNordeste.X) + ',' + FloatToStr(PontoNordeste.Y) + ',' + FloatToStr(PontoNordeste.Z) + ') - (' + FloatToStr(PontoNoroeste.X) + ',' + FloatToStr(PontoNoroeste.Y) + ',' + FloatToStr(PontoNoroeste.Z) + '), at (' + IntToStr(_x) + ',' + IntToStr(_y) + ',' + IntToStr(_z) + '). MinLimit: (' + IntToStr(LimiteMin.X) + ',' + IntToStr(LimiteMin.Y) + ',' + IntToStr(LimiteMin.Z) + '). MaxLimit: ' +  IntToStr(LimiteMax.X) + ',' + IntToStr(LimiteMax.Y) + ',' + IntToStr(LimiteMax.Z) + '). Center at: ' + IntToStr(PseudoCentro.X) + ',' + IntToStr(PseudoCentro.Y) + ',' + IntToStr(PseudoCentro.Z) + ') with range ' + IntToStr(Alcance) +  '.');
{$endif}
   end;

   // A formula acima tá no livro da Aura:
   // X = (P3.Y - P2.Y)(P1.Z - P2.Z) - (P1.Y - P2.Y)(P3.Z - P2.Z)
   // Y = (P3.Z - P2.Z)(P1.X - P2.X) - (P1.Z - P2.Z)(P3.X - P2.X)
   // Z = (P3.X - P2.X)(P1.Y - P2.Y) - (P1.X - P2.X)(P3.Y - P2.Y)

   // Transforma o vetor normal em vetor unitário. (Normalize em math3d)
   Normalize(VetorNormal);

   // Pra qual lado vai a normal?
   // Responderemos essa pergunta com um falso raycasting limitado.
   Centro := SetVector(x + 0.5,y + 0.5,z + 0.5);
   Posicao := SetVector(Centro.X,Centro.Y,Centro.Z);
   PosicaoOposta := SetVector(Centro.X,Centro.Y,Centro.Z);
{$ifdef RAY_LIMIT}
   PararRaioDaFrente := false;
   PararRaioOposto := false;
{$endif}
   Contador := 0;
   Direcao := 0;
   // Adicionamos aqui uma forma de prevenir que o mesmo voxel conte mais do
   // que uma vez, evitando um resultado errado.
   UltimoVisitado := SetVectorI(x,y,z);
   UltimoOpostoVisitado := SetVectorI(x,y,z);
{$ifdef RAY_LIMIT}
   while (not (PararRaioDaFrente and PararRaioOposto)) and (Contador < C_TAMANHO_RAYCASTING) do
{$else}
   while Contador < C_TAMANHO_RAYCASTING do
{$endif}
   begin
{$ifdef RAY_LIMIT}
      if not PararRaioDaFrente then
      begin
         Posicao.X := Posicao.X + VetorNormal.X;
         Posicao.Y := Posicao.Y + VetorNormal.Y;
         Posicao.Z := Posicao.Z + VetorNormal.Z;
         ValorFrente := PegarValorDoPonto(Mapa,UltimoVisitado,Posicao,PararRaioDaFrente);
      end
      else
      begin
         ValorFrente := 0;
      end;
      if not PararRaioOposto then
      begin
         PosicaoOposta.X := PosicaoOposta.X - VetorNormal.X;
         PosicaoOposta.Y := PosicaoOposta.Y - VetorNormal.Y;
         PosicaoOposta.Z := PosicaoOposta.Z - VetorNormal.Z;
         ValorOposto := PegarValorDoPonto(Mapa,UltimoOpostoVisitado,PosicaoOposta,PararRaioOposto);
      end
      else
      begin
         ValorOposto := 0;
      end;
      Direcao := Direcao + ValorFrente - ValorOposto;
      inc(Contador);
{$else}
      Posicao.X := Posicao.X + VetorNormal.X;
      Posicao.Y := Posicao.Y + VetorNormal.Y;
      Posicao.Z := Posicao.Z + VetorNormal.Z;
      PosicaoOposta.X := PosicaoOposta.X - VetorNormal.X;
      PosicaoOposta.Y := PosicaoOposta.Y - VetorNormal.Y;
      PosicaoOposta.Z := PosicaoOposta.Z - VetorNormal.Z;
      inc(Contador);
      Direcao := Direcao + PegarValorDoPonto(Mapa,UltimoVisitado,Posicao,PararRaioDaFrente) - PegarValorDoPonto(Mapa,UltimoOpostoVisitado,PosicaoOposta,PararRaioOposto);
{$endif}
   end;

   // Se a direção do vetor normal avaliado tiver mais peso do que a oposta
   // é porque estamos indo para dentro do volume e não para fora.
   If Direcao > 0 then
   begin
      VetorNormal.X := -VetorNormal.X;
      VetorNormal.Y := -VetorNormal.Y;
      VetorNormal.Z := -VetorNormal.Z;
   end;

   // Pega a normal mais próxima da paleta de normais (Norm2IndexXXX em Voxel_Tools)
   if Voxel.Tailer.Unknown = 4 then
      V.Normal := Norm2IndexRA2(VetorNormal)
   else
      V.Normal := Norm2IndexTS(VetorNormal);

   // Aplica a nova normal
   Voxel.SetVoxel(_x,_y,_z,V);
end;



///////////////////////////////////////////////////////////////////////
///////////////// Detecção de Superficies /////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////
/////////////////
///////
procedure AdicionaNaListaSuperficie(const Mapa: TVoxelMap; const Filtro : TFiltroDistancia; x,y,z,xdir,ydir,zdir,XFiltro,YFiltro,ZFiltro: integer; var Lista,Direcao : C3DPointList; var LimiteMin,LimiteMax : TVector3i; var MapaDeVisitas,MapaDeSuperficie : TBooleanMap);
begin
   if PontoValido(x,y,z,High(Mapa),High(Mapa[0]),High(Mapa[0,0])) then
   begin
      if PontoValido(xFiltro,yFiltro,zFiltro,High(Filtro),High(Filtro[0]),High(Filtro[0,0])) then
      begin
         if not MapaDeVisitas[XFiltro,YFiltro,ZFiltro] then
         begin
            MapaDeVisitas[XFiltro,YFiltro,ZFiltro] := true;
            if (Mapa[x,y,z] >= PESO_SUPERFICIE) and  ((Filtro[XFiltro,YFiltro,ZFiltro].X <> 0) or (Filtro[XFiltro,YFiltro,ZFiltro].Y <> 0) or (Filtro[XFiltro,YFiltro,ZFiltro].Z <> 0)) then
            begin
               Lista.Add(x,y,z);
               Direcao.Add(xdir,ydir,zdir);
               if XFiltro > LimiteMax.X then
               begin
                  LimiteMax.X := XFiltro;
               end
               else if XFiltro < LimiteMin.X then
               begin
                  LimiteMin.X := XFiltro;
               end;
               if YFiltro > LimiteMax.Y then
               begin
                  LimiteMax.Y := YFiltro;
               end
               else if YFiltro < LimiteMin.Y then
               begin
                  LimiteMin.Y := YFiltro;
               end;
               if ZFiltro > LimiteMax.Z then
               begin
                  LimiteMax.Z := ZFiltro;
               end
               else if ZFiltro < LimiteMin.Z then
               begin
                  LimiteMin.Z := ZFiltro;
               end;
               MapaDeSuperficie[XFiltro,YFiltro,ZFiltro] := true;
            end
            else
            begin
               MapaDeSuperficie[XFiltro,YFiltro,ZFiltro] := false;
            end;
         end;
      end;
   end;
end;

procedure DetectarSuperficieContinua(var MapaDaSuperficie: TBooleanMap; const Mapa : TVoxelMap; const Filtro : TFiltroDistancia; x,y,z : integer; const PontoMin,PontoMax : TVector3i; var LimiteMin,LimiteMax : TVector3i);
var
   xx,yy,zz : integer;
   xdir,ydir,zdir : integer;
   Ponto : TVector3i;
   Lista,Direcao : C3DPointList;
   MapaDeVisitas : TBooleanMap;
   Meio : integer;
begin
   // Reseta elementos
   Lista := C3DPointList.Create;
   Direcao := C3DPointList.Create;
   SetLength(MapaDeVisitas,High(Filtro)+1,High(Filtro[0])+1,High(Filtro[0,0])+1);
   for xx := Low(Filtro) to High(Filtro) do
      for yy := Low(Filtro[0]) to High(Filtro[0]) do
         for zz := Low(Filtro[0,0]) to High(Filtro[0,0]) do
         begin
            MapaDaSuperficie[xx,yy,zz] := false;
            MapaDeVisitas[xx,yy,zz] := false;
         end;
   Meio := High(Filtro) shr 1;
   MapaDaSuperficie[Meio,Meio,Meio] := true;
   MapaDeVisitas[Meio,Meio,Meio] := true;

   // Adiciona os elementos padrões, os vinte e poucos vizinhos cúbicos.
   // Prioridade de eixos: z, y, x. Centro leva prioridade sobre as diagonais.
{
   AdicionaNaListaSuperficie(Mapa,Filtro,x+1,y,z,1,0,0,Meio+1,Meio,Meio,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x-1,y,z,-1,0,0,Meio-1,Meio,Meio,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x,y+1,z,0,1,0,Meio,Meio+1,Meio,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x+1,y+1,z,1,1,0,Meio+1,Meio+1,Meio,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x-1,y+1,z,-1,1,0,Meio-1,Meio+1,Meio,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x,y-1,z,0,-1,0,Meio,Meio-1,Meio,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x+1,y-1,z,1,-1,0,Meio+1,Meio-1,Meio,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x-1,y-1,z,-1,-1,0,Meio-1,Meio-1,Meio,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x,y,z+1,0,0,1,Meio,Meio,Meio+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x+1,y,z+1,1,0,1,Meio+1,Meio,Meio+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x-1,y,z+1,-1,0,1,Meio-1,Meio,Meio+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x,y+1,z+1,0,1,1,Meio,Meio+1,Meio+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x+1,y+1,z+1,1,1,1,Meio+1,Meio+1,Meio+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x-1,y+1,z+1,-1,1,1,Meio-1,Meio+1,Meio+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x,y-1,z+1,0,-1,1,Meio,Meio-1,Meio+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x+1,y-1,z+1,1,-1,1,Meio+1,Meio-1,Meio+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x-1,y-1,z+1,-1,-1,1,Meio-1,Meio-1,Meio+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x,y,z-1,0,0,-1,Meio,Meio,Meio-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x+1,y,z-1,1,0,-1,Meio+1,Meio,Meio-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x-1,y,z-1,-1,0,-1,Meio-1,Meio,Meio-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x,y+1,z-1,0,1,-1,Meio,Meio+1,Meio-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x+1,y+1,z-1,1,1,-1,Meio+1,Meio+1,Meio-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x-1,y+1,z-1,-1,1,-1,Meio-1,Meio+1,Meio-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x,y-1,z-1,0,-1,-1,Meio,Meio-1,Meio-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x+1,y-1,z-1,1,-1,-1,Meio+1,Meio-1,Meio-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
   AdicionaNaListaSuperficie(Mapa,Filtro,x-1,y-1,z-1,-1,-1,-1,Meio-1,Meio-1,Meio-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
}

//   AdicionaNaListaSuperficie(Mapa,Filtro,x,y,z,0,0,0,Meio,Meio,Meio,Lista,Direcao,MapaDeVisitas,MapaDaSuperficie);
   Lista.Add(x,y,z);
   Direcao.Add(0,0,0);

   while Lista.GetPosition(xx,yy,zz) do
   begin
      Direcao.GetPosition(xdir,ydir,zdir);

      // Acha o ponto atual no filtro.
      Ponto.X := xx - PontoMin.X;
      Ponto.Y := yy - PontoMin.Y;
      Ponto.Z := zz - PontoMin.Z;

      // Vamos conferir o que adicionaremos a partir da direcao que ele veio
      // O eixo z leva prioridade, em seguida o eixo y e finalmente o eixo x.

      // Se um determinado eixo tem valor 1 ou -1, significa que o raio parte
      // na direcao daquele eixo. Enquanto, no caso de ele ser 0, ele pode ir
      // pra qualquer lado.

      // Primeiro os elementos centrais.
      if xdir <> -1 then
      begin
         AdicionaNaListaSuperficie(Mapa,Filtro,xx+1,yy,zz,1,ydir,zdir,Ponto.X+1,Ponto.Y,Ponto.Z,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
      end;
      if (xdir <> 1) then
      begin
         AdicionaNaListaSuperficie(Mapa,Filtro,xx-1,yy,zz,-1,ydir,zdir,Ponto.X-1,Ponto.Y,Ponto.Z,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
      end;
      if ydir <> -1 then
      begin
         AdicionaNaListaSuperficie(Mapa,Filtro,xx,yy+1,zz,xdir,1,zdir,Ponto.X,Ponto.Y+1,Ponto.Z,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         if xdir <> -1 then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx+1,yy+1,zz,1,1,zdir,Ponto.X+1,Ponto.Y+1,Ponto.Z,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         end;
         if (xdir <> 1) then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx-1,yy+1,zz,-1,1,zdir,Ponto.X-1,Ponto.Y+1,Ponto.Z,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         end;
      end;
      if ydir <> 1 then
      begin
         AdicionaNaListaSuperficie(Mapa,Filtro,xx,yy-1,zz,xdir,-1,zdir,Ponto.X,Ponto.Y-1,Ponto.Z,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         if xdir <> -1 then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx+1,yy-1,zz,1,-1,zdir,Ponto.X+1,Ponto.Y-1,Ponto.Z,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         end;
         if (xdir <> 1) then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx-1,yy-1,zz,-1,-1,zdir,Ponto.X-1,Ponto.Y-1,Ponto.Z,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         end;
      end;

      // Agora adiciona z = 1
      if zdir <> -1 then
      begin
         AdicionaNaListaSuperficie(Mapa,Filtro,xx,yy,zz+1,xdir,ydir,1,Ponto.X,Ponto.Y,Ponto.Z+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         if xdir <> -1 then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx+1,yy,zz+1,1,ydir,1,Ponto.X+1,Ponto.Y,Ponto.Z+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         end;
         if (xdir <> 1) then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx-1,yy,zz+1,1,ydir,1,Ponto.X-1,Ponto.Y,Ponto.Z+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         end;
         if ydir <> -1 then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx,yy+1,zz+1,xdir,1,1,Ponto.X,Ponto.Y+1,Ponto.Z+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            if xdir <> -1 then
            begin
               AdicionaNaListaSuperficie(Mapa,Filtro,xx+1,yy+1,zz+1,1,1,1,Ponto.X+1,Ponto.Y+1,Ponto.Z+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            end;
            if (xdir <> 1) then
            begin
               AdicionaNaListaSuperficie(Mapa,Filtro,xx-1,yy+1,zz+1,-1,1,1,Ponto.X-1,Ponto.Y+1,Ponto.Z+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            end;
         end
         else if (ydir <> 1) then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx,yy-1,zz+1,xdir,-1,1,Ponto.X,Ponto.Y-1,Ponto.Z+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            if xdir = 1 then
            begin
               AdicionaNaListaSuperficie(Mapa,Filtro,xx+1,yy-1,zz+1,1,-1,1,Ponto.X+1,Ponto.Y-1,Ponto.Z+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            end;
            if (xdir = -1) then
            begin
               AdicionaNaListaSuperficie(Mapa,Filtro,xx-1,yy-1,zz+1,-1,-1,1,Ponto.X-1,Ponto.Y-1,Ponto.Z+1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            end;
         end;
      end;
      // Agora adiciona z = -1
      if (z <> 1) then
      begin
         AdicionaNaListaSuperficie(Mapa,Filtro,xx,yy,zz-1,xdir,ydir,-1,Ponto.X,Ponto.Y,Ponto.Z-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         if (xdir <> -1) then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx+1,yy,zz-1,-1,ydir,-1,Ponto.X+1,Ponto.Y,Ponto.Z-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         end;
         if (xdir <> 1) then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx-1,yy,zz-1,-1,ydir,-1,Ponto.X-1,Ponto.Y,Ponto.Z-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
         end;
         if (ydir <> -1) then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx,yy+1,zz-1,xdir,1,-1,Ponto.X,Ponto.Y+1,Ponto.Z-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            if (xdir <> -1) then
            begin
               AdicionaNaListaSuperficie(Mapa,Filtro,xx+1,yy+1,zz-1,1,1,-1,Ponto.X+1,Ponto.Y+1,Ponto.Z-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            end
            else if (xdir <> 1) then
            begin
               AdicionaNaListaSuperficie(Mapa,Filtro,xx-1,yy+1,zz-1,-1,1,-1,Ponto.X-1,Ponto.Y+1,Ponto.Z-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            end;
         end;
         if (ydir <> 1) then
         begin
            AdicionaNaListaSuperficie(Mapa,Filtro,xx,yy-1,zz-1,xdir,-1,-1,Ponto.X,Ponto.Y-1,Ponto.Z-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            if (xdir <> -1) then
            begin
               AdicionaNaListaSuperficie(Mapa,Filtro,xx+1,yy-1,zz-1,1,-1,-1,Ponto.X+1,Ponto.Y-1,Ponto.Z-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            end;
            if (xdir <> 1) then
            begin
               AdicionaNaListaSuperficie(Mapa,Filtro,xx-1,yy-1,zz-1,-1,-1,-1,Ponto.X-1,Ponto.Y-1,Ponto.Z-1,Lista,Direcao,LimiteMin,LimiteMax,MapaDeVisitas,MapaDaSuperficie);
            end;
         end;
      end;

      Lista.GoToNextElement;
      Direcao.GoToNextElement;
   end;
   Lista.Free;
   Direcao.Free;
end;


procedure DetectarSuperficieEsferica(var MapaDaSuperficie: TBooleanMap; const Mapa : TVoxelMap; const Filtro : TFiltroDistancia; const PontoMin,PontoMax: TVector3i; var LimiteMin,LimiteMax : TVector3i);
var
   xx,yy,zz : integer;
   Ponto : TVector3i;
begin
   for xx := PontoMin.X to PontoMax.X do
      for yy := PontoMin.Y to PontoMax.Y do
         for zz := PontoMin.Z to PontoMax.Z do
         begin
            // Acha o ponto atual no filtro.
            Ponto.X := xx - PontoMin.X;
            Ponto.Y := yy - PontoMin.Y;
            Ponto.Z := zz - PontoMin.Z;
            // Confere a presença dele na superfície e no alcance do filtro
            if (Mapa[xx,yy,zz] >= PESO_SUPERFICIE) and ((Filtro[Ponto.X,Ponto.Y,Ponto.Z].X <> 0) or (Filtro[Ponto.X,Ponto.Y,Ponto.Z].Y <> 0) or (Filtro[Ponto.X,Ponto.Y,Ponto.Z].Z <> 0)) then
            begin
{$ifdef LIMITES}
               if Ponto.X > LimiteMax.X then
               begin
                  LimiteMax.X := Ponto.X;
               end
               else if Ponto.X < LimiteMin.X then
               begin
                  LimiteMin.X := Ponto.X;
               end;
               if Ponto.Y > LimiteMax.Y then
               begin
                  LimiteMax.Y := Ponto.Y;
               end
               else if Ponto.Y < LimiteMin.Y then
               begin
                  LimiteMin.Y := Ponto.Y;
               end;
               if Ponto.Z > LimiteMax.Z then
               begin
                  LimiteMax.Z := Ponto.Z;
               end
               else if Ponto.Z < LimiteMin.Z then
               begin
                  LimiteMin.Z := Ponto.Z;
               end;
{$ENDIF}
               MapaDaSuperficie[Ponto.X,Ponto.Y,Ponto.Z] := true;
            end
            else
            begin
               MapaDaSuperficie[Ponto.X,Ponto.Y,Ponto.Z] := false;
            end;
         end;
end;

///////////////////////////////////////////////////////////////////////
///////////////// Plano Tangente  /////////////////////////////////////
///////////////////////////////////////////////////////////////////////
//////////////////////////
////////////////
///////

procedure AcharPlanoTangenteEmXY(const Mapa : TBooleanMap; const Filtro : TFiltroDistancia; Alcance : integer; Meio : TVector3i; var PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste : TVector3f; const LimiteMin, LimiteMax : TVector3i);
var
   xx,yy,zz : integer;
begin
   // Resetamos os pontos do plano
   PontoSudoeste := SetVector(0,0,0);
   PontoNoroeste := SetVector(0,0,0);
   PontoSudeste := SetVector(0,0,0);
   PontoNordeste := SetVector(0,0,0);

   // Vamos achar os quatro pontos da tangente.
   // Ponto 1: Sudoeste. (X <= Meio e Y <= Meio)
   for xx := LimiteMin.X to Meio.X do
      for yy := LimiteMin.Y to Meio.Y do
         for zz := LimiteMin.Z to LimiteMax.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoSudoeste.X := PontoSudoeste.X + Filtro[xx,yy,zz].X;
               PontoSudoeste.Y := PontoSudoeste.Y + Filtro[xx,yy,zz].Y;
               PontoSudoeste.Z := PontoSudoeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Ponto 2: Noroeste. (X <= Meio e Y >= Meio)
   for xx := LimiteMin.X to Meio.X do
      for yy := Alcance to LimiteMax.Y do
         for zz := LimiteMin.Z to LimiteMax.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoNoroeste.X := PontoNoroeste.X + Filtro[xx,yy,zz].X;
               PontoNoroeste.Y := PontoNoroeste.Y + Filtro[xx,yy,zz].Y;
               PontoNoroeste.Z := PontoNoroeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Ponto 3: Sudeste. (X >= Meio e Y <= Meio)
   for xx := Alcance to LimiteMax.X do
      for yy := LimiteMin.Y to Meio.Y do
         for zz := LimiteMin.Z to LimiteMax.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoSudeste.X := PontoSudeste.X + Filtro[xx,yy,zz].X;
               PontoSudeste.Y := PontoSudeste.Y + Filtro[xx,yy,zz].Y;
               PontoSudeste.Z := PontoSudeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Ponto 4: Nordeste. (X >= Meio e Y >= Meio)
   for xx := Alcance to LimiteMax.X do
      for yy := Alcance to LimiteMax.Y do
         for zz := LimiteMin.Z to LimiteMax.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoNordeste.X := PontoNordeste.X + Filtro[xx,yy,zz].X;
               PontoNordeste.Y := PontoNordeste.Y + Filtro[xx,yy,zz].Y;
               PontoNordeste.Z := PontoNordeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;
end;

procedure AcharPlanoTangenteEmYZ(const Mapa : TBooleanMap; const Filtro : TFiltroDistancia; Alcance : integer; Meio : TVector3i; var PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste : TVector3f; const LimiteMin, LimiteMax : TVector3i);
var
   xx,yy,zz : integer;
begin
   // Resetamos os pontos do plano
   PontoSudoeste := SetVector(0,0,0);
   PontoNoroeste := SetVector(0,0,0);
   PontoSudeste := SetVector(0,0,0);
   PontoNordeste := SetVector(0,0,0);

   // Ponto 1: Sudoeste. (Y <= Meio e Z <= Meio)
   for xx := LimiteMin.X to LimiteMax.X do
      for yy := LimiteMin.Y to Meio.Y do
         for zz := LimiteMin.Z to Meio.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoSudoeste.X := PontoSudoeste.X + Filtro[xx,yy,zz].X;
               PontoSudoeste.Y := PontoSudoeste.Y + Filtro[xx,yy,zz].Y;
               PontoSudoeste.Z := PontoSudoeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Ponto 2: Noroeste. (Y <= Meio e Z >= Meio)
   for xx := LimiteMin.X to LimiteMax.X do
      for yy := LimiteMin.Y to Meio.Y do
         for zz := Alcance to LimiteMax.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoNoroeste.X := PontoNoroeste.X + Filtro[xx,yy,zz].X;
               PontoNoroeste.Y := PontoNoroeste.Y + Filtro[xx,yy,zz].Y;
               PontoNoroeste.Z := PontoNoroeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Ponto 3: Sudeste. (Y >= Meio e Z <= Meio)
   for xx := LimiteMin.X to LimiteMax.X do
      for yy := Alcance to LimiteMax.Y do
         for zz := LimiteMin.Z to Meio.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoSudeste.X := PontoSudeste.X + Filtro[xx,yy,zz].X;
               PontoSudeste.Y := PontoSudeste.Y + Filtro[xx,yy,zz].Y;
               PontoSudeste.Z := PontoSudeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Ponto 4: Nordeste. (Y >= Meio e Z >= Meio)
   for xx := LimiteMin.X to LimiteMax.X do
      for yy := Alcance to LimiteMax.Y do
         for zz := Alcance to LimiteMax.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoNordeste.X := PontoNordeste.X + Filtro[xx,yy,zz].X;
               PontoNordeste.Y := PontoNordeste.Y + Filtro[xx,yy,zz].Y;
               PontoNordeste.Z := PontoNordeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;
end;

procedure AcharPlanoTangenteEmXZ(const Mapa : TBooleanMap; const Filtro : TFiltroDistancia; Alcance : integer; Meio : TVector3i; var PontoSudoeste,PontoNoroeste,PontoSudeste,PontoNordeste : TVector3f; const LimiteMin, LimiteMax : TVector3i);
var
   xx,yy,zz : integer;
begin
   // Resetamos os pontos do plano
   PontoSudoeste := SetVector(0,0,0);
   PontoNoroeste := SetVector(0,0,0);
   PontoSudeste := SetVector(0,0,0);
   PontoNordeste := SetVector(0,0,0);

   // Ponto 1: Sudoeste. (X <= Meio e Z <= Meio)
   for xx := LimiteMin.X to Meio.X do
      for yy := LimiteMin.Y to LimiteMax.Y do
         for zz := LimiteMin.Z to Meio.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoSudoeste.X := PontoSudoeste.X + Filtro[xx,yy,zz].X;
               PontoSudoeste.Y := PontoSudoeste.Y + Filtro[xx,yy,zz].Y;
               PontoSudoeste.Z := PontoSudoeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Ponto 2: Noroeste. (X <= Meio e Z >= Meio)
   for xx := LimiteMin.X to Meio.X do
      for yy := LimiteMin.Y to LimiteMax.Y do
         for zz := Alcance to LimiteMax.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoNoroeste.X := PontoNoroeste.X + Filtro[xx,yy,zz].X;
               PontoNoroeste.Y := PontoNoroeste.Y + Filtro[xx,yy,zz].Y;
               PontoNoroeste.Z := PontoNoroeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Ponto 3: Sudeste. (X >= Meio e Z <= Meio)
   for xx := Alcance to LimiteMax.X do
      for yy := LimiteMin.Y to LimiteMax.Y do
         for zz := LimiteMin.Z to Meio.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoSudeste.X := PontoSudeste.X + Filtro[xx,yy,zz].X;
               PontoSudeste.Y := PontoSudeste.Y + Filtro[xx,yy,zz].Y;
               PontoSudeste.Z := PontoSudeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;

   // Ponto 4: Nordeste. (X >= Meio e Z >= Meio)
   for xx := Alcance to LimiteMax.X do
      for yy := LimiteMin.Y to LimiteMax.Y do
         for zz := Alcance to LimiteMax.Z do
         begin
            if Mapa[xx,yy,zz] then
            begin
               // Aplica o filtro no ponto (xx,yy,zz)
               PontoNordeste.X := PontoNordeste.X + Filtro[xx,yy,zz].X;
               PontoNordeste.Y := PontoNordeste.Y + Filtro[xx,yy,zz].Y;
               PontoNordeste.Z := PontoNordeste.Z + Filtro[xx,yy,zz].Z;
            end;
         end;
end;



///////////////////////////////////////////////////////////////////////
///////////////// Outras Funções  /////////////////////////////////////
///////////////////////////////////////////////////////////////////////
//////////////////////////
////////////////
///////

// Verifica se o ponto é válido.
function PontoValido (const x,y,z,maxx,maxy,maxz : integer) : boolean;
begin
   result := false;
   if (x < 0) or (x > maxx) then exit;
   if (y < 0) or (y > maxy) then exit;
   if (z < 0) or (z > maxz) then exit;
   result := true;
end;

// Pega o valor no ponto do mapa para o falso raytracing em AplicarFiltro.
function PegarValorDoPonto(const Mapa : TVoxelMap; var Ultimo : TVector3i; const Ponto : TVector3f; var EstaNoVazio : boolean): single;
var
   PontoI : TVector3i;
begin
   PontoI := SetVectorI(Trunc(Ponto.X),Trunc(Ponto.Y),Trunc(Ponto.Z));
   Result := 0;
   if PontoValido(PontoI.X,PontoI.Y,PontoI.Z,High(Mapa),High(Mapa[0]),High(Mapa[0,0])) then
   begin
      if (Ultimo.X <> PontoI.X) or (Ultimo.Y <> PontoI.Y) or (Ultimo.Z <> PontoI.Z) then
      begin
         Ultimo.X := PontoI.X;
         Ultimo.Y := PontoI.Y;
         Ultimo.Z := PontoI.Z;
         Result := Mapa[PontoI.X,PontoI.Y,PontoI.Z];
         if Result >= PESO_PARTE_DO_VOLUME then
            Result := PESO_SUPERFICIE;
         EstaNoVazio := (Result <> PESO_SUPERFICIE);
      end;
   end
   else
      EstaNoVazio := true;
end;


end.
