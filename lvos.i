*                            dos.library                             *

_LVOOpen			EQU -30
_LVOClose			EQU -36
_LVORead			EQU -42
_LVOWrite			EQU -48
_LVOInput			EQU -54
_LVOOutput			EQU -60
_LVOSeek			EQU -66
_LVODeleteFile			EQU -72
_LVORename			EQU -78
_LVOLock			EQU -84
_LVOUnLock			EQU -90
_LVODupLock			EQU -96
_LVOExamine			EQU -102
_LVOExNext			EQU -108
_LVOInfo			EQU -114
_LVOCreateDir			EQU -120
_LVOCurrentDir			EQU -126
_LVOIoErr			EQU -132
_LVOCreateProc			EQU -138
_LVOExit			EQU -144
_LVOLoadSeg			EQU -150
_LVOUnLoadSeg			EQU -156
_LVODeviceProc			EQU -174
_LVOSetComment			EQU -180
_LVOSetProtection		EQU -186
_LVODateStamp			EQU -192
_LVODelay			EQU -198
_LVOWaitForChar			EQU -204
_LVOParentDir			EQU -210
_LVOIsInteractive		EQU -216
_LVOExecute			EQU -222
_LVOAllocDosObject		EQU -228
_LVOFreeDosObject		EQU -234
_LVODoPkt			EQU -240
_LVOSendPkt			EQU -246
_LVOWaitPkt			EQU -252
_LVOReplyPkt			EQU -258
_LVOAbortPkt			EQU -264
_LVOLockRecord			EQU -270
_LVOLockRecords			EQU -276
_LVOUnLockRecord		EQU -282
_LVOUnLockRecords		EQU -288
_LVOSelectInput			EQU -294
_LVOSelectOutput		EQU -300
_LVOFGetC			EQU -306
_LVOFPutC			EQU -312
_LVOUnGetC			EQU -318
_LVOFRead			EQU -324
_LVOFWrite			EQU -330
_LVOFGets			EQU -336
_LVOFPuts			EQU -342
_LVOVFWritef			EQU -348
_LVOVFPrintf			EQU -354
_LVOFlush			EQU -360
_LVOSetVBuf			EQU -366
_LVODupLockFromFH		EQU -372
_LVOOpenFromLock		EQU -378
_LVOParentOfFH			EQU -384
_LVOExamineFH			EQU -390
_LVOSetFileDate			EQU -396
_LVONameFromLock		EQU -402
_LVONameFromFH			EQU -408
_LVOSplitName			EQU -414
_LVOSameLock			EQU -420
_LVOSetMode			EQU -426
_LVOExAll			EQU -432
_LVOReadLink			EQU -438
_LVOMakeLink			EQU -444
_LVOChangeMode			EQU -450
_LVOSetFileSize			EQU -456
_LVOSetIoErr			EQU -462
_LVOFault			EQU -468
_LVOPrintFault			EQU -474
_LVOErrorReport			EQU -480
_LVOCli				EQU -492
_LVOCreateNewProc		EQU -498
_LVORunCommand			EQU -504
_LVOGetConsoleTask		EQU -510
_LVOSetConsoleTask		EQU -516
_LVOGetFileSysTask		EQU -522
_LVOSetFileSysTask		EQU -528
_LVOGetArgStr			EQU -534
_LVOSetArgStr			EQU -540
_LVOFindCliProc			EQU -546
_LVOMaxCli			EQU -552
_LVOSetCurrentDirName		EQU -558
_LVOGetCurrentDirName		EQU -564
_LVOSetProgramName		EQU -570
_LVOGetProgramName		EQU -576
_LVOSetPrompt			EQU -582
_LVOGetPrompt			EQU -588
_LVOSetProgramDir		EQU -594
_LVOGetProgramDir		EQU -600
_LVOSystemTagList		EQU -606
_LVOAssignLock			EQU -612
_LVOAssignLate			EQU -618
_LVOAssignPath			EQU -624
_LVOAssignAdd			EQU -630
_LVORemAssignList		EQU -636
_LVOGetDeviceProc		EQU -642
_LVOFreeDeviceProc		EQU -648
_LVOLockDosList			EQU -654
_LVOUnLockDosList		EQU -660
_LVOAttemptLockDosList		EQU -666
_LVORemDosEntry			EQU -672
_LVOAddDosEntry			EQU -678
_LVOFindDosEntry		EQU -684
_LVONextDosEntry		EQU -690
_LVOMakeDosEntry		EQU -696
_LVOFreeDosEntry		EQU -702
_LVOIsFileSystem		EQU -708
_LVOFormat			EQU -714
_LVORelabel			EQU -720
_LVOInhibit			EQU -726
_LVOAddBuffers			EQU -732
_LVOCompareDates		EQU -738
_LVODateToStr			EQU -744
_LVOStrToDate			EQU -750
_LVOInternalLoadSeg		EQU -756
_LVOInternalUnLoadSeg		EQU -762
_LVONewLoadSeg			EQU -768
_LVOAddSegment			EQU -774
_LVOFindSegment			EQU -780
_LVORemSegment			EQU -786
_LVOCheckSignal			EQU -792
_LVOReadArgs			EQU -798
_LVOFindArg			EQU -804
_LVOReadItem			EQU -810
_LVOStrToLong			EQU -816
_LVOMatchFirst			EQU -822
_LVOMatchNext			EQU -828
_LVOMatchEnd			EQU -834
_LVOParsePattern		EQU -840
_LVOMatchPattern		EQU -846
_LVOFreeArgs			EQU -858
_LVOFilePart			EQU -870
_LVOPathPart			EQU -876
_LVOAddPart			EQU -882
_LVOStartNotify			EQU -888
_LVOEndNotify			EQU -894
_LVOSetVar			EQU -900
_LVOGetVar			EQU -906
_LVODeleteVar			EQU -912
_LVOFindVar			EQU -918
_LVOCliInitNewcli		EQU -930
_LVOCliInitRun			EQU -936
_LVOWriteChars			EQU -942
_LVOPutStr			EQU -948
_LVOVPrintf			EQU -954
_LVOParsePatternNoCase		EQU -966
_LVOMatchPatternNoCase		EQU -972
_LVOSameDevice			EQU -984
_LVOExAllEnd			EQU -990
_LVOSetOwner			EQU -996

*                            exec.library                            *

_LVOSupervisor			EQU -30
_LVOInitCode			EQU -72
_LVOInitStruct			EQU -78
_LVOMakeLibrary			EQU -84
_LVOMakeFunctions		EQU -90
_LVOFindResident		EQU -96
_LVOInitResident		EQU -102
_LVOAlert			EQU -108
_LVODebug			EQU -114
_LVODisable			EQU -120
_LVOEnable			EQU -126
_LVOForbid			EQU -132
_LVOPermit			EQU -138
_LVOSetSR			EQU -144
_LVOSuperState			EQU -150
_LVOUserState			EQU -156
_LVOSetIntVector		EQU -162
_LVOAddIntServer		EQU -168
_LVORemIntServer		EQU -174
_LVOCause			EQU -180
_LVOAllocate			EQU -186
_LVODeallocate			EQU -192
_LVOAllocMem			EQU -198
_LVOAllocAbs			EQU -204
_LVOFreeMem			EQU -210
_LVOAvailMem			EQU -216
_LVOAllocEntry			EQU -222
_LVOFreeEntry			EQU -228
_LVOInsert			EQU -234
_LVOAddHead			EQU -240
_LVOAddTail			EQU -246
_LVORemove			EQU -252
_LVORemHead			EQU -258
_LVORemTail			EQU -264
_LVOEnqueue			EQU -270
_LVOFindName			EQU -276
_LVOAddTask			EQU -282
_LVORemTask			EQU -288
_LVOFindTask			EQU -294
_LVOSetTaskPri			EQU -300
_LVOSetSignal			EQU -306
_LVOSetExcept			EQU -312
_LVOWait			EQU -318
_LVOSignal			EQU -324
_LVOAllocSignal			EQU -330
_LVOFreeSignal			EQU -336
_LVOAllocTrap			EQU -342
_LVOFreeTrap			EQU -348
_LVOAddPort			EQU -354
_LVORemPort			EQU -360
_LVOPutMsg			EQU -366
_LVOGetMsg			EQU -372
_LVOReplyMsg			EQU -378
_LVOWaitPort			EQU -384
_LVOFindPort			EQU -390
_LVOAddLibrary			EQU -396
_LVORemLibrary			EQU -402
_LVOOldOpenLibrary		EQU -408
_LVOCloseLibrary		EQU -414
_LVOSetFunction			EQU -420
_LVOSumLibrary			EQU -426
_LVOAddDevice			EQU -432
_LVORemDevice			EQU -438
_LVOOpenDevice			EQU -444
_LVOCloseDevice			EQU -450
_LVODoIO			EQU -456
_LVOSendIO			EQU -462
_LVOCheckIO			EQU -468
_LVOWaitIO			EQU -474
_LVOAbortIO			EQU -480
_LVOAddResource			EQU -486
_LVORemResource			EQU -492
_LVOOpenResource		EQU -498
_LVORawDoFmt			EQU -522
_LVOGetCC			EQU -528
_LVOTypeOfMem			EQU -534
_LVOProcure			EQU -540
_LVOVacate			EQU -546
_LVOOpenLibrary			EQU -552
_LVOInitSemaphore		EQU -558
_LVOObtainSemaphore		EQU -564
_LVOReleaseSemaphore		EQU -570
_LVOAttemptSemaphore		EQU -576
_LVOObtainSemaphoreList		EQU -582
_LVOReleaseSemaphoreList	EQU -588
_LVOFindSemaphore		EQU -594
_LVOAddSemaphore		EQU -600
_LVORemSemaphore		EQU -606
_LVOSumKickData			EQU -612
_LVOAddMemList			EQU -618
_LVOCopyMem			EQU -624
_LVOCopyMemQuick		EQU -630
_LVOCacheClearU			EQU -636
_LVOCacheClearE			EQU -642
_LVOCacheControl		EQU -648
_LVOCreateIORequest		EQU -654
_LVODeleteIORequest		EQU -660
_LVOCreateMsgPort		EQU -666
_LVODeleteMsgPort		EQU -672
_LVOObtainSemaphoreShared	EQU -678
_LVOAllocVec			EQU -684
_LVOFreeVec			EQU -690
_LVOCreatePool			EQU -696
_LVODeletePool			EQU -702
_LVOAllocPooled			EQU -708
_LVOFreePooled			EQU -714
_LVOAttemptSemaphoreShared	EQU -720
_LVOColdReboot			EQU -726
_LVOStackSwap			EQU -732
_LVOChildFree			EQU -738
_LVOChildOrphan			EQU -744
_LVOChildStatus			EQU -750
_LVOChildWait			EQU -756
_LVOCachePreDMA			EQU -762
_LVOCachePostDMA		EQU -768
_LVOAddMemHandler		EQU -774
_LVORemMemHandler		EQU -780
