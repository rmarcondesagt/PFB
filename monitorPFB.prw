#INCLUDE "FWMBROWSE.CH"
#INCLUDE "FWMVCDEF.CH"
#INCLUDE 'PROTHEUS.CH'
#INCLUDE 'TOTVS.CH'


User Function zOO25()
	Local aCoors	    :=	FwGetDialogSize()
	Local bClose        :=  {|| oDlgLog:DeActivate(), If( Type("oTempTable") == "O", oTempTable:Delete(),)}
	Local bViagens      :=  {|| FWMsgRun(, {|| TMSAF60()}, "Viagens", "Processando dados aguarde...")}
	Local bPainel       :=  {|| FWMsgRun(, {|| TMSAF76()}, "Painel de agendamentos", "Processando dados aguarde...")}
	Local bAgenda       :=  {|| FWMsgRun(, {|| TMSAF05()}, "Agendamento de viagens", "Processando dados aguarde...")}
	Local bAtualiza     :=  {|| FWMsgRun(, {|| fDadosGrid2(),fGrid(),fConsGrid1(),fCriaGrid1(oGrp2),oBrwLog:Activate()}, "Atualizando", "Atualizando tela...")}
	Local nX 			:=	0

	Private dDateDe     :=  dDataBase
	Private dDateAt     :=  dDataBase
	Private oPanel      :=  Nil
	Private oLayerMain  :=  Nil
	Private oBrwLog     :=  Nil
	Private oDlgLog     :=  Nil
	Private oPanelCol1  :=  Nil
	Private oPanelCol2  :=  Nil
	Private oGrid       :=  Nil
	Private oPanelEsp   :=  Nil
	Private oSay      	:=  Nil
	Private oGet      	:=  Nil
	Private oCombo 		:=	Nil
	Private oComboQ 	:=	Nil
	Private oGrp1 		:=	Nil
	Private oGrp2 		:=	Nil
	Private oTempTable 	:=	Nil
	Private oCheck1 	:=	Nil
	Private oZP3 		:=	Nil//EBParamZP3():New()

	Private lCheck 		:=	.T.

	Private aBackUp     :=  {}
	Private aBackUpCli  :=  {}
	Private aBackUpSeg  :=  {}
	Private aEstados 	:=	{}
	Private aItens      :=  {}
	Private aCodMrc		:=	{}//U_EBFAT562('fMarcas')
	Private aColunas    :=  fColunas()
	Private aRegiao 	:=	{}//StrToKarr( RTrim( oZP3:ZP3GetDados("ZZ_REGIAO_PED") ), ";")//fRegiao()

	Private aTipoCli 	:=	{}//fTpCliente()
	Private aTpCli      :=  {""}

	Private cMoeda		:=	""
	Private cTpMercado	:=	""
	Private cViaFab 	:=	""
	Private cAprovado 	:=	""
	Private cTpProd		:=	""
	Private cTipoCli	:=	""
	Private cTempTable 	:=	""
	Private cValidPed 	:=	""
	Private cRegiao 	:=	""
	Private cAliasTemp 	:=	GetNextAlias()

	Private aItenGrid1  :=  {}

	Begin Sequence

		For nX := 1 to Len(aTipoCli)
			aAdd(aTpCli,aTipoCli[nX])
		Next

		If Type("oDlgLog") == "O"
			oDlgLog:DeActivate()
		EndIf

		oDlgLog :=  FWDialogModal():New()
		oDlgLog:SetBackground( .T. )
		oDlgLog:SetTitle( 'Monitor TMS' )
		oDlgLog:SetSize( (aCoors[3]/2),(aCoors[4]/2))
		oDlgLog:EnableFormBar( .T. )
		oDlgLog:SetCloseButton( .T. )
		oDlgLog:SetEscClose( .T. )
		oDlgLog:CreateDialog()
		oDlgLog:CreateFormBar()
		oDlgLog:AddButton("Viagens"     		,bViagens 	, "Viagens"	        	,, .T., .F., .T., )
		oDlgLog:AddButton("Legendas"        	,bClose  	, "Legandas"		    ,, .T., .F., .T., )
		oDlgLog:AddButton("Agendamento"     	,bAgenda 	, "Agendamento"			,, .T., .F., .T., )
		oDlgLog:AddButton("Filtro"          	,bClose  	, "Filtro"		        ,, .T., .F., .T., )
		oDlgLog:AddButton("Painel Agendamento"	,bPainel 	, "PainelAgendamento"	,, .T., .F., .T., )
		oDlgLog:AddButton("Atualizar"			,bAtualiza	, "Atualizar"			,, .T., .F., .T., )

		oPanel := oDlgLog:GetPanelMain()

		oLayer := FWLayer():New()
		olayer:init(oPanel)
		oLayer:AddLine('LIN1', 50, .F.)
		oLayer:AddLine('LIN2', 50, .F.)
		oLayer:AddCollumn("FILTERS" , 40,.F.,'LIN1')
		oLayer:AddCollumn("ESPEC" 	, 60,.F.,'LIN1')
		oLayer:AddCollumn("DADOS"   , 100,.F.,'LIN2')
		oLayer:addWindow("DADOS"    ,"L_Win02","Tabela Operação Logistica",100,.F.,.F.,,'LIN2')

		oPanelCol1	:=	oLayer:getColPanel('FILTERS','LIN1')
		oPanelEsp	:=	oLayer:getColPanel('ESPEC'	,'LIN1')
		oPanelCol2  :=	oLayer:getWinPanel('DADOS'   ,'L_Win02','LIN2')

		nAltura		:= Int((oPanelCol1:nBottom - oPanelCol1:nTop)/2)
		nLargura	:= Int( (oPanelCol1:nRight - oPanelCol1:nLeft)/2)

		nHeight		:= Int((oPanelEsp:nBottom - oPanelEsp:nTop)/2)
		nWidth		:= Int((oPanelEsp:nBottom - oPanelEsp:nTop)/2)

		fDadosGrid2()
		fGrid()
		fCampos()

		oBrwLog:Activate()
		oDlgLog:Activate()

	End Sequence

Return

Static function fCampos()
	Local oBtn      :=  Nil
	Local bConsulta :=  {|| FwMsgRun(, {|| aItens := fItensPed(), fGrid()},"","Gerando Dados....")}
	Local bTreport  :=  {|| If(Empty(aItens),MsgStop("Primeiro realize a consulta dos dados"),(/*fItensPed(),*/fReport()))}

	Begin Sequence

		//Criação de grupos para separar na tela.
		oGrp1	:=	TGroup():New( 002, 002, nAltura, nLargura , "", oPanelCol1,,, .T., .F.)
		oGrp2 	:=	TGroup():New( 002, 002, nAltura, 570 , "", oPanelEsp,,, .T., .F.)

		fConsGrid1()
		fCriaGrid1(oGrp2)


	End Sequence

Return

Static function fConsGrid1()

	Local cQuery    :=  ""
	Local cAlias    :=  GetNextAlias()

	cQuery := "select" +CRLF
	cQuery += "DF1_FILIAL ,DF1_FILORI ,DF1_CODUNP ,DF1_NUMAGE ,DF0_CODOBC ,DF1_DATPRC  ,DF1_HORPRC ,DUA_DATOCO ,DUA_HOROCO"   +CRLF
	cQuery += ",DF1_XDTINC ,DF1_XHRCOL ,DF1_XDTFCO ,DF1_XHFCOL ,DF1_XUFCOL ,DF1_LOCCOL ,DF1_XCIDCL ,DF1_XLOCEN ,DF1_XNATCA"   +CRLF
	cQuery += ",DF1_CLIDEV,DF1_XCIDEN,DF1_DTENT,DF1_HRENT ,DF1_XFIMEN ,DF1_XSITCO,DF1_XVEIC1,DF1_XCDMOT,DF1_XUFENT,DF1_XDOCP" +CRLF
	cQuery += ",DF0_XHRCAD,DF0_DATCAD,DF1_XSTAT ,DF1_PESO,DF1_METRO3,DF1_PESOM3,DF1_QTDVOL,DF2_CODEMB,DF1_UNDESC,DF1_VALMER"  +CRLF
	cQuery += ",DF1_XSITCO,DTQ_ROTA,DA8_DESC,DF1_XKM,DF1_XRECB "+CRLF

	cQuery += "from " + RetSqlName("DF1") + " F1" +CRLF

	cQuery += "INNER JOIN " + RetSqlName("DF0") + " F0 ON (F1.DF1_FILIAL = F0.DF0_FILIAL AND F1.DF1_NUMAGE = F0.DF0_NUMAGE AND F1.D_E_L_E_T_<>'*' AND F0.D_E_L_E_T_<>'*')"+CRLF
	cQuery += "INNER JOIN " + RetSqlName("DUA") + " UA ON (F1.DF1_FILIAL = UA.DUA_FILIAL AND UA.D_E_L_E_T_<>'*'AND F1.D_E_L_E_T_<>'*' AND DF1_FILIAL+DF1_FILDOC+DF1_DOC+DF1_SERIE = DUA_FILIAL+DUA_FILDOC+DUA_DOC+DUA_SERIE)"+CRLF
	cQuery += "INNER JOIN " + RetSqlName("DF2") + " F2 ON (F1.DF1_FILIAL = F2.DF2_FILIAL AND F1.DF1_NUMAGE = F2.DF2_NUMAGE AND F2.D_E_L_E_T_<>'*'AND F1.D_E_L_E_T_<>'*')"+CRLF
	cQuery += "INNER JOIN " + RetSqlName("DTQ") + " DT ON (DT.DTQ_FILIAL = F1.DF1_FILIAL AND DT.DTQ_FILORI = F1.DF1_FILORI AND DT.D_E_L_E_T_<>'*'AND F1.D_E_L_E_T_<>'*' AND DUA_VIAGEM = DTQ_VIAGEM)"+CRLF
	cQuery += "INNER JOIN " + RetSqlName("DA8") + " A8 ON (F1.DF1_FILIAL = A8.DA8_FILIAL AND A8.DA8_COD = DT.DTQ_ROTA )"+CRLF

	cQuery += "WHERE DUA_CODOCO <> 'ENT'" +CRLF

	cQuery += "GROUP BY DF1_FILIAL,DF1_FILORI,DF1_CODUNP,DF1_NUMAGE,DF0_CODOBC,DF1_DATPRC,DF1_HORPRC,DUA_DATOCO,DUA_HOROCO,DF1_XDTINC,DF1_XHRCOL"+CRLF
	cQuery += ",DF1_XDTFCO,DF1_XHFCOL,DF1_XUFCOL,DF1_LOCCOL,DF1_XCIDCL,DF1_XLOCEN,DF1_XNATCA,DF1_CLIDEV,DF1_XCIDEN,DF1_DTENT,DF1_HRENT" +CRLF
	cQuery += ",DF1_XFIMEN,DF1_XSITCO,DF1_XVEIC1,DF1_XCDMOT,DF1_XUFENT,DF1_XDOCP,DF0_XHRCAD,DF0_DATCAD,DF1_XSTAT,DF1_PESO,DF1_METRO3" +CRLF
	cQuery += ",DF1_PESOM3,DF1_QTDVOL,DF2_CODEMB,DF1_UNDESC,DF1_VALMER,DF1_XSITCO,DTQ_ROTA,DA8_DESC,DF1_XKM,DF1_XRECB"


	cQuery  :=  ChangeQuery(cQuery)
	MemoWrite( "c:\temp\qrytms1.txt", cQuery )
	cAlias  :=  MPSysOpenQuery(cQuery)

	aItenGrid1 := {}

	//Grava na temporária
	(cAlias)->(DbGoTop())
	While !(cAlias)->(EoF())

		aAdd(aItenGrid1,{;
			.F.                             ,;  //1
		xFilial()                       ,;  //2
		alltrim((cAlias)->DF1_FILORI)    ,;  //3
		(cAlias)->DF1_CODUNP            ,;  //4
		(cAlias)->DF1_NUMAGE            ,;  //5
		(cAlias)->DF0_CODOBC            ,;  //6
		Stod((cAlias)->DF1_DATPRC)       ,;  //7
		(cAlias)->DF1_HORPRC            ,;  //8
		(cAlias)->DF1_XDTINC            ,;  //9
		(cAlias)->DF1_XHRCOL            ,;  //10
		(cAlias)->DUA_DATOCO            ,;  //11
		(cAlias)->DUA_HOROCO            ,;  //12
		(cAlias)->DF1_XDTFCO            ,;  //13
		(cAlias)->DF1_XHFCOL            ,;  //14
		(cAlias)->DF1_XUFCOL            ,;  //15
		(cAlias)->DF1_LOCCOL            ,;  //16
		(cAlias)->DF1_XCIDCL            ,;  //17
		(cAlias)->DF1_CLIDEV            ,;  //18
		(cAlias)->DF1_XCIDEN            ,;  //19
		(cAlias)->DF1_XLOCEN            ,;  //20
		(cAlias)->DF1_DTENT             ,;  //21
		(cAlias)->DF1_HRENT             ,;  //22
		(cAlias)->DF1_XNATCA            ,;  //23
		(cAlias)->DF1_XFIMEN            ,;  //24
		(cAlias)->DF1_XCDMOT            ,;  //25
		(cAlias)->DF1_XSITCO            ,;  //26
		(cAlias)->DF1_XVEIC1            ,;  //27
		(cAlias)->DF1_XUFENT            ,;  //28
		(cAlias)->DF1_XDOCP             ,;  //29
		(cAlias)->DF0_XHRCAD            ,;  //30
		(cAlias)->DF0_DATCAD            ,;  //31
		(cAlias)->DF1_XSTAT             ,;  //32
		(cAlias)->DF1_PESO              ,;  //33
		(cAlias)->DF1_PESOM3            ,;  //34
		(cAlias)->DF1_QTDVOL            ,;  //35
		(cAlias)->DF2_CODEMB            ,;  //36
		(cAlias)->DF1_VALMER            ,;  //37
		(cAlias)->DF1_UNDESC            ,;  //38
		(cAlias)->DTQ_ROTA              ,;  //39
		(cAlias)->DA8_DESC              ,;  //40
		(cAlias)->DF1_XKM               ,;  //41
		(cAlias)->DF1_XRECB             ;   //42
		})

		(cAlias)->(DbSkip())
	EndDo

	(cAlias)->(DbCloseArea())

Return

Static function fCriaGrid1(oGrp2)
	Local bMarca        :=  {|| fMarkColum() }

	//*******************************************************************//
	//** Criando o grid dos campos MARKBROWSE                          **//
	//*******************************************************************//

	Begin Sequence

		If Type("oBrwLog") == "O"// .and. !lRet
			oBrwLog:DeActivate( .T. )
		EndIf

		oBrwLog:= FWBrowse():New(oGrp2)

		oBrwLog:SetDataArray()
		oBrwLog:SetArray( aItenGrid1 )
		oBrwLog:DisableSeek()
		oBrwLog:DisableReport()
		oBrwLog:DisableSaveConfig()
		// oBrwLog:SetDoubleClick( {|| fDupClique() } )

		Add MarkColumn oColumn DATA {|| If(aItenGrid1[oBrwLog:nAt,1],'LBOK', 'LBNO')} DoubleClick bMarca HeaderClick {|| aEval(aItenGrid1,{|x| x[1] := !x[1]}),oBrwLog:Refresh(.T.)} Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(2)  + "]}")  Title "FILIAL"                                       Picture X3Picture("DF1_FILIAL") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(3)  + "]}")  Title GetSx3Cache( "DF1_FILORI", "X3_TITULO"   )     Picture X3Picture("DF1_FILORI") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(4)  + "]}")  Title GetSx3Cache( "DF1_CODUNP", "X3_TITULO"   )     Picture X3Picture("DF1_CODUNP") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(5)  + "]}")  Title GetSx3Cache( "DF1_NUMAGE", "X3_TITULO"   )     Picture X3Picture("DF1_NUMAGE") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(6)  + "]}")  Title GetSx3Cache( "DF0_CODOBC", "X3_TITULO"   )     Picture X3Picture("DF0_CODOBC") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(7)  + "]}")  Title GetSx3Cache( "DF1_DATPRC", "X3_TITULO"   )     Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(8)  + "]}")  Title GetSx3Cache( "DF1_HORPRC ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(9)  + "]}")  Title GetSx3Cache( "DF1_XDTINC ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(10)  + "]}") Title GetSx3Cache( "DF1_XHRCOL ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(11)  + "]}") Title GetSx3Cache( "DUA_DATOCO ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(12)  + "]}") Title GetSx3Cache( "DUA_HOROCO ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(13)  + "]}") Title GetSx3Cache( "DF1_XDTFCO ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(14)  + "]}") Title GetSx3Cache( "DF1_XHFCOL ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(15)  + "]}") Title GetSx3Cache( "DF1_XUFCOL ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(16)  + "]}") Title GetSx3Cache( "DF1_LOCCOL ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(17)  + "]}") Title GetSx3Cache( "DF1_XCIDCL ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(18)  + "]}") Title GetSx3Cache( "DF1_CLIDEV ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(19)  + "]}") Title GetSx3Cache( "DF1_XCIDEN ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(20)  + "]}") Title GetSx3Cache( "DF1_XLOCEN ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(21)  + "]}") Title GetSx3Cache( "DF1_DTENT  ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(22)  + "]}") Title GetSx3Cache( "DF1_HRENT  ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(23)  + "]}") Title GetSx3Cache( "DF1_XNATCA ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(24)  + "]}") Title GetSx3Cache( "DF1_XFIMEN ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(25)  + "]}") Title GetSx3Cache( "DF1_XCDMOT ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(26)  + "]}") Title GetSx3Cache( "DF1_XSITCO ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(27)  + "]}") Title GetSx3Cache( "DF1_XVEIC1 ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(28)  + "]}") Title GetSx3Cache( "DF1_XUFENT ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(29)  + "]}") Title GetSx3Cache( "DF1_XDOCP  ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(30)  + "]}") Title GetSx3Cache( "DF0_XHRCAD ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(31)  + "]}") Title GetSx3Cache( "DF0_DATCAD ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(32)  + "]}") Title GetSx3Cache( "DF1_XSTAT  ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(33)  + "]}") Title GetSx3Cache( "DF1_PESO   ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(34)  + "]}") Title GetSx3Cache( "DF1_PESOM3 ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(35)  + "]}") Title GetSx3Cache( "DF1_QTDVOL ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(36)  + "]}") Title GetSx3Cache( "DF2_CODEMB ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(37)  + "]}") Title GetSx3Cache( "DF1_VALMER ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(38)  + "]}") Title GetSx3Cache( "DF1_UNDESC ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(39)  + "]}") Title GetSx3Cache( "DTQ_ROTA   ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(40)  + "]}") Title GetSx3Cache( "DA8_DESC   ", "X3_TITULO"   )    Picture X3Picture("F4_ZZCLASS") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(41)  + "]}") Title GetSx3Cache( "DF1_XKM    ", "X3_TITULO"   )    Picture X3Picture("DF1_XKM") Of oBrwLog
		Add Column oColumn Data &("{|| aItenGrid1[oBrwLog:nAt][" + Str(42)  + "]}") Title GetSx3Cache( "DF1_XRECB  ", "X3_TITULO"   )    Picture X3Picture("DF1_XRECB") Of oBrwLog

	End Sequence

Return

static Function fDupClique()
	Local aArea   := FWGetArea()
	Local cGrupo  := aItenGrid1[oBrwLog:nAt][5]
	// Local Item
	Local cFunBkp := FunName()

	//Se a pergunta for confirmada, abre a visualização do grupo
	If FWAlertYesNo("Deseja alterar o agendamento '" + cGrupo + "'?", "Continua?")
		DbSelectArea('DF0')
		DF0->(DbSetOrder(1)) //Filial + AGENDAMENTO + ITEM


		//Se conseguir posicionar
		If DF0->(DbSeek(FWxFilial('DF0')+cGrupo))
			SetFunName("TMSAF05")
			FWExecView('Alterar','TMSAF05()', MODEL_OPERATION_UPDATE)
			SetFunName(cFunBkp)
		EndIf

		// IF lOk	FWAlertSuccess("Alterado com suscesso", "Concluido")else	FWAlertWarning("Cancelado pelo operador","Cancelado")Endif


	EndIf

	FWRestArea(aArea)
Return

Static Function fMarkColum()

	If !aItenGrid1[oBrwLog:nAT][1]
		aItenGrid1[oBrwLog:nAT][1] :=	.T.
	Else
		aItenGrid1[oBrwLog:nAT][1] :=	.F.
	EndIf

Return

Static function fItensPed()

Return aDados

Static function fTempTable()

Return

Static function fGrid()
	Local nZ    :=  0

	Begin sequence

		If Type("oGrid") == "O"
			oGrid:DeActivate(.T.)
		EndIf

		oGrid := FWBrowse():New(oPanelCol2)

		oGrid:SetDataArray()
		oGrid:SetArray( aItens )
		oGrid:DisableSeek()
		oGrid:DisableReport()
		oGrid:AddLegend("Empty(aItens[oGrid:nAt][6])"															, "BR_VERDE"			, "Disponível")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '1' .and. (AllTrim(aItens[oGrid:nAt][25]) $ '48|49')"	, "BR_CINZA"			, "Inicio de Viagem Carregado")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '2' .and. (AllTrim(aItens[oGrid:nAt][25]) $ '48|49')"	, "BR_CINZA"			, "Inicio de Viagem Vazio")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '50' .and. (AllTrim(aItens[oGrid:nAt][25]) $ '48|49')", "BR_CINZA"			, "MINERVA - INICIO DE VIAGEM CARREGADO")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '55' .and. (AllTrim(aItens[oGrid:nAt][25]) == '057')"	, "BR_VERMELHO"			, "MINERVA - CHEGADA AGUARDANDO DESCARGA")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '3' .and. (AllTrim(aItens[oGrid:nAt][25]) == '057')"	, "BR_VERMELHO"			, "Chegada Cliente - Aguardando Descarga")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '4' .and. (AllTrim(aItens[oGrid:nAt][25]) == '057')"	, "BR_VERMELHO"			, "Chegada no Cliente  - Aguardando Carregamento")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC05'"													, "BR_AZUL"				, "Inicio de Descarga")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC06'"													, "BR_AMARELO"			, "Inicio de Carregamento")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '7' .and. (AllTrim(aItens[oGrid:nAt][25]) == '050')"	, "BR_PRETO"			, "Fim de Descarga")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC08'"													, "BR_PINK"				, "Fim de Carregamento")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC09'"													, "BR_VIOLETA"			, "Proxima Entrega")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC10'"													, "BR_PRETO_0"			, "Reinicio de Viagem Carregado")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC11'"													, "BR_PRETO_1"			, "Reinicio de Viagem Vazio")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC12'"													, "BR_PRETO_3"			, "Mudanca de Destino Vazio")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC13'"													, "BR_CANCEL"			, "Mudanca de Destino Carregado")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC14'"													, "BR_VERDE_ESCURO"		, "Aguardando Liberacao Documentacao Carregado")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC15'"													, "BR_MARROM_OCEAN"		, "Aguardando Liberacao Documentacao Vazio")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC16'"													, "BR_AZUL_CLARO"		, "Aguardando Programacao")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC23'"													, "BR_AZUL_ESCURO"		, "Parada (Residência)")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC24'"													, "BR_VERMELHO_ESCURO"	, "Parado na Estrada - Congestionamento/Acidente")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC25'"													, "BR_LARANJA"			, "Parada - Posto Fiscal (Documentacao Carga/Veiculo)")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC26'"													, "BR_LARANJA_ESCURO"	, "Parada - Manutencao")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC27'"													, "BR_LARANJA_CLARO"	, "Parado - Sinistro")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC28'"													, "BR_AMARELO_ESCURO"	, "Aduana - Entrada")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC29'"													, "BR_AMARELO_CLARO"	, "Aduana Saida CARREGADO")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC30'"													, "BR_VERMELHO_CLARO"	, "Aduana - Saida Vazio")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC31'"													, "BR_MAGENTA"			, "Recado")
		//oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == 'MC33'"													, "BR_MAGENTA_CLARO"	, "Manutenção Corretiva")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '34' .and. (AllTrim(aItens[oGrid:nAt][25]) == '057')"	, "BR_PINK"				, "INICIO DE DESCARGA FRIGORIFICADA")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '35' .and. (AllTrim(aItens[oGrid:nAt][25]) == '057')"	, "BR_PINK"				, "INICIO DE CARREGAMENTO FRIGORIFICADA")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '36' .and. (AllTrim(aItens[oGrid:nAt][25]) == '056')"	, "BR_MARROM_OCEAN"		, "FIM DE DESCARGA FRIGORIFICA")
		oGrid:AddLegend("AllTrim(aItens[oGrid:nAt][6]) == '56' .and. (AllTrim(aItens[oGrid:nAt][25]) == '056')"	, "BR_MARROM_OCEAN"	, "Legenda")
		oGrid:DisableSaveConfig()
		oGrid:SetEditCell(.F.)
		oGrid:SetInsert(.F.)

		For nZ := 1 To Len(aColunas)
			Add Column oColumn;
				Data &("{|| aItens[oGrid:nAt][" + Str(nZ) + "]}");
				Title   aColunas[nZ][2] ;
				Size    aColunas[nZ][3] ;
				Picture aColunas[nZ][4] Of oGrid
		Next

		oGrid:Activate()

	End Sequence

Return

Static function fColunas()
	Local nX        :=  0
	Local aStruct   :=  {}
	Local aColunas  :=  {;
		"DF1_DOC"		,;
		"DTC_DOC"		,;
		"DTX_MANIFE"	,;
		"DF0_NUMAGE"	,;
		"DA3_PLACA" 	,;
		"Z1_MACRO" 		,;
		"Z49_DESC" 		,;
		"A1_END"		,;
		"A1_MUN"		,;
		"B1_DESC"		,;
		"DF1_DATPRC"	,;
		"DF1_HORPRC"	,;
		"Z49_DESC"		,;
		"A1_NOME"		,;
		"Z1_MSGTIME"	,;
		"Z43_SETPON"	,;
		"Z43_ARRET"		,;
		"DTX_DATMAN"	,;
		"DTX_HORMAN"	,;
		"DTR_DATFIM"	,;
		"DTR_HORFIM"	,;
		"T9_PROPRIE"	,;
		"DF1_DATPRE"	,;
		"DF1_HORPRE"	,;
		"DTW_ATIVID"	,;
		"DF1_XVEIC1"	,;
		"DF1_XVEIC2"	,;
		"DF1_XVEIC3"	;
		}

	For nX  := 1 to Len(aColunas)
		aAdd(aStruct,{aColunas[nX],;
			GetSx3Cache( aColunas[nX], "X3_TITULO" )	,;
			GetSx3Cache( aColunas[nX], "X3_TAMANHO")	,;
			GetSx3Cache( aColunas[nX], "X3_PICTURE")	;
			})
	Next

Return aStruct
//CHAMA ROTINA TMSA350 APONTAMENTO DE VIAGEM
User Function ExcAut350()
Static Function fNewStatus(cFilOri,cViagem,cCliente,cLoja,cMacro)
	Local aOrd 		:=	SaveOrd({"DTW"})
	Local aCabDTW  	:=	{}
	Local nOpcao    :=	3
	Local cAtivid	:=	""
	Local nX 		:=	0
	Local nQtdAtiv	:=	0
	Local aAtivid	:=	{}

	DTW->( DbSetOrder( 8 ) )
	DTW->( DbSeek( xFilial('DTW') + cFilOri + cViagem) )

// Dados para o apontamento da operação da viagem

	If AllTrim(cMacro) == "1" .or. AllTrim(cMacro) == "2" .or. AllTrim(cMacro) == "50"
		nQtdAtiv 	:= 2
		cAtivid		:= "048;049"
		aAtivid		:=	StrTokArr(cAtivid, ';')
	ElseIf AllTrim(cMacro) == "3" .or. AllTrim(cMacro) == "34" .or. AllTrim(cMacro) == "35" .or. AllTrim(cMacro) == "55"
		nQtdAtiv 	:= 1
		cAtivid		:= "057"
		aAtivid		:=	StrTokArr(cAtivid, ';')
	ElseIf AllTrim(cMacro) == "7"
		nQtdAtiv 	:= 1
		cAtivid		:= "050"
		aAtivid		:=	StrTokArr(cAtivid, ';')
	ElseIf AllTrim(cMacro) == "36" .or. AllTrim(cMacro) == "56"
		nQtdAtiv 	:= 1
		cAtivid		:= "056"
		aAtivid		:=	StrTokArr(cAtivid, ';')
	EndIf

	For nX := 1 To nQtdAtiv
		aCabDTW	:=	{}
		Aadd( aCabDTW, { 'DTW_DATINI' , dDataBase								, Nil } )    //Data Inicial da Operação
		Aadd( aCabDTW, { 'DTW_HORINI' , SubStr(Time(),1,2) + SubStr(Time(),4,2) , Nil } )    //Hora inicial da Operação
		Aadd( aCabDTW, { 'DTW_DATREA' , dDataBase                               , Nil } )    //Data final da Operação
		Aadd( aCabDTW, { 'DTW_HORREA' , SubStr(Time(),1,2) + SubStr(Time(),4,2) , Nil } )    //Hora final da Operação
		Aadd( aCabDTW, { 'DTW_ATIVID' , aAtivid[nX]								, Nil } )    //Atividade
		Aadd( aCabDTW, { 'DTW_CODCLI' , cCliente								, Nil } )    //Atividade
		Aadd( aCabDTW, { 'DTW_LOJCLI' , cLoja									, Nil } )    //Atividade

		// Executa rotina TMSA350
		MsExecAuto({|a,b,c,d,e,f,g|TMSA350(a,b,c,d,e,f,g)},,,,,, aCabDTW, nOpcao)

	Next

	RestOrd(aOrd,.T.)
Return

Static function fDadosGrid2()
	Local cQuery	:=	""
	Local cAlias	:=	""
	Local nX 		:=	0

	cQuery 	:= " Select "																					+	CRLF
	cQuery 	+= "	DF1.DF1_DOC, "																			+	CRLF
	cQuery 	+= "	DTC.DTC_DOC, "																			+	CRLF
	cQuery 	+= "	DTX.DTX_MANIFE,"																		+	CRLF
	cQuery 	+= "	DF1.DF1_NUMAGE, "																		+	CRLF
	cQuery 	+= "	DA3.DA3_PLACA, "																		+	CRLF
	cQuery 	+= "	SZ1.Z1_MACRO, "																			+	CRLF
	cQuery 	+= "	Z49.Z49_DESC, "																			+	CRLF
	cQuery 	+= "	SA1.A1_END,"																			+	CRLF
	cQuery 	+= "	SA1.A1_MUN,"																			+	CRLF
	cQuery 	+= "	SB1.B1_DESC,"																			+	CRLF
	cQuery 	+= "	DF1.DF1_DATPRC,"																		+	CRLF
	cQuery 	+= "	DF1.DF1_HORPRC,"																		+	CRLF
	cQuery 	+= "	Z49.Z49_DESC,"																			+	CRLF
	cQuery 	+= "	SA1.A1_NOME,"																			+	CRLF
	cQuery 	+= "	SZ1.Z1_MSGTIME,"																		+	CRLF
	cQuery 	+= "	Z43.Z43_SETPON,"																		+	CRLF
	cQuery 	+= "	Z43.Z43_ARRET,"																			+	CRLF
	cQuery 	+= "	DTX.DTX_DATMAN,"																		+	CRLF
	cQuery 	+= " 	DTX.DTX_HORMAN,"																		+	CRLF
	cQuery 	+= "	DTR.DTR_DATFIM,"																		+	CRLF
	cQuery 	+= "	DTR.DTR_HORFIM,"																		+	CRLF
	cQuery 	+= "	ST9.T9_PROPRIE,"																		+	CRLF
	cQuery 	+= "	DF1.DF1_DATPRE,"																		+	CRLF
	cQuery 	+= "	DF1.DF1_HORPRE,"																		+	CRLF
	cQuery 	+= "	DTW.DTW_ATIVID,"																		+	CRLF
	cQuery 	+= " 	DF1.DF1_XVEIC1,"																		+	CRLF
	cQuery 	+= " 	DF1.DF1_XVEIC2,"																		+	CRLF
	cQuery 	+= " 	DF1.DF1_XVEIC3,"																		+	CRLF
	cQuery 	+= " 	DF1.DF1_FILORI,"																		+	CRLF
	cQuery 	+= " 	DF1.DF1_CLIREM,"																		+	CRLF
	cQuery 	+= " 	DF1.DF1_LOJREM"																			+	CRLF

	cQuery 	+= " From " + RetSqlName("DA3") + " DA3 " 														+	CRLF

	cQuery 	+= " Left Join ( Select * From " + RetSqlName("DF1") + " (NoLock) where D_E_L_E_T_ = '') DF1"	+	CRLF
	cQuery 	+= " 	On DF1.DF1_FILIAL = '" + xFilial("DF1") + "'"											+	CRLF
	cQuery 	+= " 	And DF1.DF1_XVEIC1 = DA3.DA3_COD"														+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("DF0") + " (NoLock) where D_E_L_E_T_ = '') DF0" 	+	CRLF
	cQuery 	+= "		ON DF1.DF1_FILIAL = DF0.DF0_FILIAL" 												+	CRLF
	cQuery 	+= "		AND DF1.DF1_NUMAGE = DF0.DF0_NUMAGE" 												+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("DTC") + " (NoLock) where D_E_L_E_T_ = '') DTC" 	+	CRLF
	cQuery 	+= " 	ON DTC.DTC_FILIAL = DF0.DF0_FILIAL" 													+	CRLF
	cQuery 	+= " 	AND DTC.DTC_NUMSOL = DF0.DF0_NUMAGE"													+	CRLF
	cQuery 	+= " 	And DTC.DTC_FILCFS = DF0.DF0_FILIAL"													+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("SZ1") + " (NoLock) where D_E_L_E_T_ = '') SZ1"	+	CRLF
	cQuery 	+= " 	ON SZ1.Z1_FILIAL = '" + xFilial("SZ1") + "'"											+	CRLF
	cQuery 	+= " 	AND SZ1.Z1_PLACA = DA3.DA3_PLACA"														+	CRLF
	cQuery 	+= " 	AND SZ1.Z1_CODMOT = DA3.DA3_MOTORI "													+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("DTX") + " (NoLock) where D_E_L_E_T_ = '') DTX"	+	CRLF
	cQuery 	+= " 	ON DTX.DTX_FILIAL = DF0.DF0_FILIAL" 													+	CRLF
	cQuery 	+= " 	AND DTX.DTX_VIAGEM = DF0.DF0_NUMAGE"													+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("DTQ") + " (NoLock) where D_E_L_E_T_ = '') DTQ" 	+	CRLF
	cQuery 	+= " 	ON DTQ.DTQ_FILIAL = DF1.DF1_FILIAL" 													+	CRLF
	cQuery 	+= " 	AND DTQ.DTQ_FILORI = DF1.DF1_FILORI" 													+	CRLF
	cQuery 	+= " 	And DTQ.DTQ_VIAGEM = DF1.DF1_NUMAGE" 													+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("Z49") + " (NoLock) where D_E_L_E_T_ = '') Z49" 	+	CRLF
	cQuery 	+= " 	ON Z49.Z49_FILIAL = SZ1.Z1_FILIAL" 														+	CRLF
	cQuery 	+= " 	AND Z49.Z49_COD = SZ1.Z1_MACRO"															+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("DA8") + " (NoLock) where D_E_L_E_T_ = '') DA8" 	+	CRLF
	cQuery 	+= " 	ON DF1.DF1_FILIAL = DA8.DA8_FILIAL"														+	CRLF
	cQuery 	+= " 	AND DA8.DA8_COD = DTQ.DTQ_ROTA" 														+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("SA1") + " (NoLock) where D_E_L_E_T_ = '') SA1" 	+	CRLF
	cQuery 	+= " 	ON SA1.A1_FILIAL = DF1.DF1_FILIAL"														+	CRLF
	cQuery 	+= " 	AND SA1.A1_COD = DF1.DF1_CLIREM"														+	CRLF
	cQuery 	+= " 	AND SA1.A1_LOJA = DF1.DF1_LOJREM"	 													+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("DF2") + " (NoLock) where D_E_L_E_T_ = '') DF2"	+	CRLF
	cQuery 	+= " 	ON DF2.DF2_FILIAL = DF1.DF1_FILIAL "													+	CRLF
	cQuery 	+= " 	AND DF2.DF2_NUMAGE = DF1.DF1_NUMAGE "													+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("SB1") + " (NoLock) where D_E_L_E_T_ = '') SB1"	+	CRLF
	cQuery 	+= " 	ON SB1.B1_FILIAL = DF2.DF2_FILIAL"														+	CRLF
	cQuery 	+= " 	AND SB1.B1_COD = DF2.DF2_CODPRO"														+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("Z43") + " (NoLock) where D_E_L_E_T_ = '') Z43"	+	CRLF
	cQuery 	+= " 	ON Z43.Z43_FILIAL = DF1.DF1_FILIAL"														+	CRLF
	cQuery 	+= " 	And Z43.Z43_PLACA = DA3.DA3_PLACA"														+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("DTR") + " (NoLock) where D_E_L_E_T_ = '') DTR"	+	CRLF
	cQuery 	+= " 	ON DTR.DTR_FILIAL = DF1.DF1_FILIAL"														+	CRLF
	cQuery 	+= " 	And DTR.DTR_VIAGEM = DF0.DF0_NUMAGE"													+	CRLF
	cQuery 	+= " 	And DTR.DTR_CODVEI = DF1.DF1_XVEIC1"													+	CRLF

	cQuery 	+= " Left Join (Select * From " + RetSqlName("ST9") + " (NoLock) where D_E_L_E_T_ = '') ST9"	+	CRLF
	cQuery 	+= " 	ON ST9.T9_FILIAL = DF1.DF1_FILIAL"														+	CRLF
	cQuery 	+= " 	And ST9.T9_CODBEM = DA3.DA3_PLACA"														+	CRLF

	cQuery	+= " Left Join (Select * From " + RetSqlName("DTW") + " (NoLock) where D_E_L_E_T_ = '') DTW"	+	CRLF
	cQuery	+= "		ON DTW.DTW_FILIAL = DF1.DF1_FILIAL"													+	CRLF
	cQuery	+= "		And DTW.DTW_FILORI = DF1.DF1_FILORI"												+	CRLF
	cQuery	+= "		And DTW.DTW_VIAGEM = DF1.DF1_NUMAGE"												+	CRLF
	cQuery	+= "		And DTW.DTW_SEQUEN = DF1.DF1_ITEAGE"												+	CRLF

	cQuery	+= "	Where "																					+	CRLF
	cQuery	+= "		DA3.D_E_L_E_T_<>'*' "																+	CRLF
	cQuery	+= "		and DA3_PLACA = 'AZD3F13' "														+	CRLF

	aItens	:=	{}

	cQuery 	:=	ChangeQuery(cQuery)
	cAlias 	:=	MPSysOpenQuery(cQuery)


	Do While (cAlias)->(!Eof())

		If AllTrim((cAlias)->Z1_MACRO) == "1" .or.;
				AllTrim((cAlias)->Z1_MACRO) == "2" .or.;
				AllTrim((cAlias)->Z1_MACRO) == "3" .or.;
				AllTrim((cAlias)->Z1_MACRO) == "4".or.;
				AllTrim((cAlias)->Z1_MACRO) == "7".or.;
				AllTrim((cAlias)->Z1_MACRO) == "34".or.;
				AllTrim((cAlias)->Z1_MACRO) == "35".or.;
				AllTrim((cAlias)->Z1_MACRO) == "36".or.;
				AllTrim((cAlias)->Z1_MACRO) == "50".or.;
				AllTrim((cAlias)->Z1_MACRO) == "55".or.;
				AllTrim((cAlias)->Z1_MACRO) == "56";
				.and. (!Empty((cAlias)->DF1_NUMAGE))
			fNewStatus((cAlias)->DF1_FILORI,(cAlias)->DF1_NUMAGE,(cAlias)->DF1_CLIREM,(cAlias)->DF1_LOJREM,(cAlias)->Z1_MACRO)
		EndIf

		(cAlias)->(DbSkip())
	EndDo

	(cAlias)->(DbCloseArea())

	cQuery 	:=	ChangeQuery(cQuery)
	cAlias 	:=	MPSysOpenQuery(cQuery)
	*/

	Do While (cAlias)->(!Eof())

		aAdd(aItens,{;
			(cAlias)->DF1_DOC		,;
			(cAlias)->DTC_DOC		,;
			(cAlias)->DTX_MANIFE	,;
			(cAlias)->DF1_NUMAGE	,;
			(cAlias)->DA3_PLACA		,;
			(cAlias)->Z1_MACRO 		,;
			(cAlias)->Z49_DESC 		,;
			(cAlias)->A1_END		,;
			(cAlias)->A1_MUN		,;
			(cAlias)->B1_DESC		,;
			(cAlias)->DF1_DATPRC	,;
			(cAlias)->DF1_HORPRC	,;
			(cAlias)->Z49_DESC		,;
			(cAlias)->A1_NOME		,;
			(cAlias)->Z1_MSGTIME	,;
			(cAlias)->Z43_SETPON	,;
			(cAlias)->Z43_ARRET		,;
			(cAlias)->DTX_DATMAN	,;
			(cAlias)->DTX_HORMAN	,;
			(cAlias)->DTR_DATFIM	,;
			(cAlias)->DTR_HORFIM	,;
			(cAlias)->T9_PROPRIE	,;
			(cAlias)->DF1_DATPRE	,;
			(cAlias)->DF1_HORPRE	,;
			(cAlias)->DTW_ATIVID	,;
			(cAlias)->DF1_XVEIC1	,;
			(cAlias)->DF1_XVEIC2	,;
			(cAlias)->DF1_XVEIC3	;
			})

		(cAlias)->(DbSkip())
	EndDo

	(cAlias)->(DbCloseArea())

Return


/*Static function fTpCliente()

	Begin Sequence 

		cTipoCli	:=	GetSx3Cache("A1_ZZTCLI","X3_CBOX")

		aTipoCli 	:=	StrToKarr(cTipoCli,";")	 
	
	End Sequence 

Return aTipoCli

Static function fDadosHist()
	Local nX 	:=	0
	Local cQry	:=	""
	
Return cQry
*/
/*user function apresentacao() 

    rpcClearEnv()
    RPCSetEnv("01", "01", "agility1", "Agility@0001", "TMS")
    Define MSDialog oMainWND from 0,0 to 600, 1350 pixel  
    @ 5,5 button "zOO25" of oMainWND pixel action u_fonteApresent()   
    Activate MSDialog oMainWND    

return
*/
