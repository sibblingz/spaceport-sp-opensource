{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Development.ActionScript3.Project where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

import Development.ActionScript3.Error
import Development.ActionScript3.Project
import qualified Development.ActionScript3.FlashBuilder as FlashBuilder
import qualified Development.ActionScript3.FlashDevelop as FlashDevelop

import QuasiQuoters

main = defaultMain tests
tests = $(testGroupGenerator) : concatMap hUnitTestToTests
  [ TestLabel "parseFlashBuilderDocument tests" tests_parseFlashBuilderDocument
  , TestLabel "parseFlashDevelopDocument tests" tests_parseFlashDevelopDocument
  , TestLabel "parseCommandNames tests" tests_parseCommandNames
  ]

tests_parseFlashDevelopDocument = TestList $ map testProject
  [ ( Right
      ( Project
      { projectSourceDirs = ["src"]
      , projectLibraryDirs = []
      , projectLibraryFiles = []
      , projectAS3EntryPoint = "src/Main.as"
      }
      , []
      )
    , [xml|
<?xml version="1.0" encoding="utf-8"?>
<project>
  <!-- Output SWF options -->
  <output>
    <movie disabled="False" />
    <movie input="" />
    <movie path="bin\GorillasAS3.swf" />
    <movie fps="30" />
    <movie width="800" />
    <movie height="600" />
    <movie version="11" />
    <movie background="#FFFFFF" />
  </output>
  <!-- Other classes to be compiled into your SWF -->
  <classpaths>
    <class path="src" />
  </classpaths>
  <!-- Build options -->
  <build>
    <option accessible="False" />
    <option allowSourcePathOverlap="False" />
    <option benchmark="False" />
    <option es="False" />
    <option locale="" />
    <option loadConfig="" />
    <option optimize="True" />
    <option showActionScriptWarnings="True" />
    <option showBindingWarnings="True" />
    <option showDeprecationWarnings="True" />
    <option showUnusedTypeSelectorWarnings="True" />
    <option strict="True" />
    <option useNetwork="True" />
    <option useResourceBundleMetadata="True" />
    <option warnings="True" />
    <option verboseStackTraces="False" />
    <option linkReport="" />
    <option loadExterns="" />
    <option staticLinkRSL="True" />
    <option additional="" />
    <option compilerConstants="" />
    <option customSDK="" />
  </build>
  <!-- SWC Include Libraries -->
  <includeLibraries>
    <!-- example: <element path="..." /> -->
  </includeLibraries>
  <!-- SWC Libraries -->
  <libraryPaths>
    <!-- example: <element path="..." /> -->
  </libraryPaths>
  <!-- External Libraries -->
  <externalLibraryPaths>
    <!-- example: <element path="..." /> -->
  </externalLibraryPaths>
  <!-- Runtime Shared Libraries -->
  <rslPaths>
    <!-- example: <element path="..." /> -->
  </rslPaths>
  <!-- Intrinsic Libraries -->
  <intrinsics>
    <!-- example: <element path="..." /> -->
  </intrinsics>
  <!-- Assets to embed into the output SWF -->
  <library>
    <!-- example: <asset path="..." id="..." update="..." glyphs="..." mode="..." place="..." sharepoint="..." /> -->
  </library>
  <!-- Class files to compile (other referenced classes will automatically be included) -->
  <compileTargets>
    <compile path="src\Main.as" />
  </compileTargets>
  <!-- Paths to exclude from the Project Explorer tree -->
  <hiddenPaths>
    <!-- example: <hidden path="..." /> -->
  </hiddenPaths>
  <!-- Executed before build -->
  <preBuildCommand />
  <!-- Executed after build -->
  <postBuildCommand alwaysRun="False" />
  <!-- Other project options -->
  <options>
    <option showHiddenPaths="False" />
    <option testMovie="Default" />
  </options>
</project>
      |]
    )
  , ( Right
      ( Project
      { projectSourceDirs = ["src"]
      , projectLibraryDirs = []
      , projectLibraryFiles = ["libs/assets.swc"]
      , projectAS3EntryPoint = "src/Main.as"
      }
      , [ BuildCommandsNotRun PreBuild ["echo"]
        , IncludingAllClassesFromSWC "libs\\assets.swc"
        ]
      )
    , [xml|
<?xml version="1.0" encoding="utf-8"?>
<project>
  <!-- Output SWF options -->
  <output>
    <movie disabled="False" />
    <movie input="" />
    <movie path="bin\GorillasAS3.swf" />
    <movie fps="30" />
    <movie width="800" />
    <movie height="600" />
    <movie version="11" />
    <movie background="#FFFFFF" />
  </output>
  <!-- Other classes to be compiled into your SWF -->
  <classpaths>
    <class path="src" />
  </classpaths>
  <!-- Build options -->
  <build>
    <option accessible="False" />
    <option allowSourcePathOverlap="False" />
    <option benchmark="False" />
    <option es="False" />
    <option locale="" />
    <option loadConfig="" />
    <option optimize="True" />
    <option showActionScriptWarnings="True" />
    <option showBindingWarnings="True" />
    <option showDeprecationWarnings="True" />
    <option showUnusedTypeSelectorWarnings="True" />
    <option strict="True" />
    <option useNetwork="True" />
    <option useResourceBundleMetadata="True" />
    <option warnings="True" />
    <option verboseStackTraces="False" />
    <option linkReport="" />
    <option loadExterns="" />
    <option staticLinkRSL="True" />
    <option additional="" />
    <option compilerConstants="" />
    <option customSDK="" />
  </build>
  <!-- SWC Include Libraries -->
  <includeLibraries>
    <!-- example: <element path="..." /> -->
  </includeLibraries>
  <!-- SWC Libraries -->
  <libraryPaths>
    <element path="libs\assets.swc" />
  </libraryPaths>
  <!-- External Libraries -->
  <externalLibraryPaths>
    <!-- example: <element path="..." /> -->
  </externalLibraryPaths>
  <!-- Runtime Shared Libraries -->
  <rslPaths>
    <!-- example: <element path="..." /> -->
  </rslPaths>
  <!-- Intrinsic Libraries -->
  <intrinsics>
    <!-- example: <element path="..." /> -->
  </intrinsics>
  <!-- Assets to embed into the output SWF -->
  <library>
    <!-- example: <asset path="..." id="..." update="..." glyphs="..." mode="..." place="..." sharepoint="..." /> -->
  </library>
  <!-- Class files to compile (other referenced classes will automatically be included) -->
  <compileTargets>
    <compile path="src\Main.as" />
  </compileTargets>
  <!-- Paths to exclude from the Project Explorer tree -->
  <hiddenPaths>
    <!-- example: <hidden path="..." /> -->
  </hiddenPaths>
  <!-- Executed before build -->
  <preBuildCommand>echo 'Pre-build command!'</preBuildCommand>
  <!-- Executed after build -->
  <postBuildCommand alwaysRun="False" />
  <!-- Other project options -->
  <options>
    <option showHiddenPaths="False" />
    <option testMovie="Default" />
  </options>
</project>
      |]
    )
  , ( Right
      ( Project
      { projectSourceDirs = ["src"]
      , projectLibraryDirs = []
      , projectLibraryFiles = []
      , projectAS3EntryPoint = "src/Main.as"
      }
      , []
      )
    , [xml|
<?xml version="1.0" encoding="utf-8"?>
<project>
  <!-- Output SWF options -->
  <output>
    <movie disabled="False" />
    <movie input="" />
    <movie path="bin/launchpadtest2.swf" />
    <movie fps="30" />
    <movie width="800" />
    <movie height="600" />
    <movie version="10" />
    <movie minorVersion="1" />
    <movie background="#FFFFFF" />
  </output>
  <!-- Other classes to be compiled into your SWF -->
  <classpaths>
    <class path="src" />
  </classpaths>
  <!-- Build options -->
  <build>
    <option accessible="False" />
    <option allowSourcePathOverlap="False" />
    <option benchmark="False" />
    <option es="False" />
    <option loadConfig="" />
    <option optimize="True" />
    <option showActionScriptWarnings="True" />
    <option showBindingWarnings="True" />
    <option showDeprecationWarnings="True" />
    <option showUnusedTypeSelectorWarnings="True" />
    <option strict="True" />
    <option useNetwork="True" />
    <option useResourceBundleMetadata="True" />
    <option warnings="True" />
    <option verboseStackTraces="False" />
    <option staticLinkRSL="True" />
    <option additional="" />
    <option customSDK="" />
  </build>
  <!-- Class files to compile (other referenced classes will automatically be included) -->
  <compileTargets>
    <compile path="src\Main.as" />
  </compileTargets>
  <!-- Paths to exclude from the Project Explorer tree -->
  <hiddenPaths>
    <!-- example: <hidden path="..." /> -->
  </hiddenPaths>
  <!-- Executed before build -->
  <preBuildCommand />
  <!-- Executed after build -->
  <postBuildCommand alwaysRun="False" />
  <!-- Other project options -->
  <options>
    <option showHiddenPaths="False" />
    <option testMovie="Default" />
  </options>
</project>
      |]
    )
  ]
  where
    testProject (expected, input)
      = FlashDevelop.parseDocument input ~?= expected

tests_parseFlashBuilderDocument = TestList $ map testProject
  [ ( Right
      ( Project
      { projectSourceDirs = ["src"]
      , projectLibraryDirs = []
      , projectLibraryFiles = []
      , projectAS3EntryPoint = "Main.as"
      }
      , []
      )
    , [xml|
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<actionScriptProperties analytics="false" mainApplicationPath="Main.as" projectUUID="aaf4ac61-e176-4f18-882e-3db830b3f3aa" version="10">
  <compiler additionalCompilerArguments="-locale en_US" autoRSLOrdering="true" copyDependentFiles="true" fteInMXComponents="false" generateAccessible="true" htmlExpressInstall="true" htmlGenerate="true" htmlHistoryManagement="true" htmlPlayerVersionCheck="true" includeNetmonSwc="false" outputFolderPath="bin-debug" removeUnusedRSL="true" sourceFolderPath="src" strict="true" targetPlayerVersion="0.0.0" useApolloConfig="false" useDebugRSLSwfs="true" verifyDigests="true" warn="true">
    <compilerSourcePath/>
    <libraryPath defaultLinkType="0">
      <libraryPathEntry kind="4" path="">
        <excludedEntries>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="1" linkType="1" path="${PROJECT_FRAMEWORKS}/locale/{locale}"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/advancedgrids.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/framework.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/mx/mx.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/netmon.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/sparkskins.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/rpc.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/datavisualization.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/videoPlayer.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/flash-integration.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_flashflexkit.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_agent.swc" useDefaultLinkType="false"/>
        </excludedEntries>
      </libraryPathEntry>
    </libraryPath>
    <sourceAttachmentPath/>
  </compiler>
  <applications>
    <application path="Main.as"/>
  </applications>
  <modules/>
  <buildCSSFiles/>
  <flashCatalyst validateFlashCatalystCompatibility="false"/>
</actionScriptProperties>
      |]
    )

  , ( Right
      ( Project
      { projectSourceDirs = ["src"]
      , projectLibraryDirs = []
      , projectLibraryFiles = []
      , projectAS3EntryPoint = "Balloons.as"
      }
      , []
      )
    , [xml|
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<actionScriptProperties analytics="false" mainApplicationPath="Balloons.as" projectUUID="2bab2afa-001b-481f-badf-28d34b1ad70c" version="10">
  <compiler additionalCompilerArguments="-locale en_US" autoRSLOrdering="true" copyDependentFiles="true" fteInMXComponents="false" generateAccessible="true" htmlExpressInstall="true" htmlGenerate="true" htmlHistoryManagement="true" htmlPlayerVersionCheck="true" includeNetmonSwc="false" outputFolderPath="bin-debug" removeUnusedRSL="true" sourceFolderPath="src" strict="true" targetPlayerVersion="0.0.0" useApolloConfig="false" useDebugRSLSwfs="true" verifyDigests="true" warn="true">
    <compilerSourcePath/>
    <libraryPath defaultLinkType="0">
      <libraryPathEntry kind="4" path="">
        <excludedEntries>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="1" linkType="1" path="${PROJECT_FRAMEWORKS}/locale/{locale}"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/advancedgrids.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/framework.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/mx/mx.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/netmon.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/sparkskins.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/rpc.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/datavisualization.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/videoPlayer.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/flash-integration.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_flashflexkit.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_agent.swc" useDefaultLinkType="false"/>
        </excludedEntries>
      </libraryPathEntry>
    </libraryPath>
    <sourceAttachmentPath/>
  </compiler>
  <applications>
    <application path="Balloons.as"/>
  </applications>
  <modules/>
  <buildCSSFiles/>
  <flashCatalyst validateFlashCatalystCompatibility="false"/>
</actionScriptProperties>
      |]
    )

  , ( Right
      ( Project
      { projectSourceDirs = ["src"]
      , projectLibraryDirs = []
      , projectLibraryFiles = []
      , projectAS3EntryPoint = "EveIndustry.as"
      }
      , []
      )
    , [xml|
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<actionScriptProperties analytics="false" mainApplicationPath="EveIndustry.as" projectUUID="34201596-fe7f-4a78-a26c-94d18b9ee95d" version="10">
  <compiler additionalCompilerArguments="-locale en_US" autoRSLOrdering="true" copyDependentFiles="true" fteInMXComponents="false" generateAccessible="true" htmlExpressInstall="true" htmlGenerate="false" htmlHistoryManagement="false" htmlPlayerVersionCheck="true" includeNetmonSwc="false" outputFolderPath="bin-debug" removeUnusedRSL="true" sourceFolderPath="src" strict="true" targetPlayerVersion="0.0.0" useApolloConfig="true" useDebugRSLSwfs="true" verifyDigests="true" warn="true">
    <compilerSourcePath/>
    <libraryPath defaultLinkType="0">
      <libraryPathEntry kind="4" path="">
        <excludedEntries>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="1" linkType="1" path="${PROJECT_FRAMEWORKS}/locale/{locale}"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/advancedgrids.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/air/airspark.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/framework.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/mx/mx.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/netmon.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/sparkskins.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/rpc.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/datavisualization.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/videoPlayer.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/flash-integration.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_flashflexkit.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/air/airframework.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_agent.swc" useDefaultLinkType="false"/>
        </excludedEntries>
      </libraryPathEntry>
    </libraryPath>
    <sourceAttachmentPath/>
  </compiler>
  <applications>
    <application path="EveIndustry.as">
      <airExcludes/>
    </application>
  </applications>
  <modules/>
  <buildCSSFiles/>
  <flashCatalyst validateFlashCatalystCompatibility="false"/>
  <buildTargets>
    <buildTarget buildTargetName="default">
      <airSettings airCertificatePath="" airTimestamp="true" anePathSet="false" version="1">
        <airExcludes/>
        <anePaths/>
      </airSettings>
      <actionScriptSettings version="1"/>
    </buildTarget>
  </buildTargets>
</actionScriptProperties>
      |]
    )

  , ( Right
      ( Project
      { projectSourceDirs = ["Code/src", "bin/assetsEmbedded"]
      , projectLibraryDirs = []
      , projectLibraryFiles = []
      , projectAS3EntryPoint = "Preloader.as"
      }
      , []
      )
    , [xml|
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<actionScriptProperties analytics="false" mainApplicationPath="Preloader.as" projectUUID="49a1e73a-7f11-46c8-8f5d-29afc19a629e" version="10">
  <compiler additionalCompilerArguments="-locale en_US" autoRSLOrdering="true" copyDependentFiles="true" fteInMXComponents="false" generateAccessible="true" htmlExpressInstall="true" htmlGenerate="true" htmlHistoryManagement="true" htmlPlayerVersionCheck="true" includeNetmonSwc="false" outputFolderPath="bin-debug" removeUnusedRSL="true" sourceFolderPath="Code/src" strict="true" targetPlayerVersion="0.0.0" useApolloConfig="false" useDebugRSLSwfs="true" verifyDigests="true" warn="true">
    <compilerSourcePath>
      <compilerSourcePathEntry kind="1" linkType="1" path="bin/assetsEmbedded"/>
    </compilerSourcePath>
    <libraryPath defaultLinkType="0">
      <libraryPathEntry kind="4" path="">
        <excludedEntries>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="1" linkType="1" path="${PROJECT_FRAMEWORKS}/locale/{locale}"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/advancedgrids.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/framework.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/mx/mx.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/netmon.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/sparkskins.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/rpc.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/videoPlayer.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/datavisualization.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/flash-integration.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_flashflexkit.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_agent.swc" useDefaultLinkType="false"/>
        </excludedEntries>
      </libraryPathEntry>
    </libraryPath>
    <sourceAttachmentPath/>
  </compiler>
  <applications>
    <application path="Preloader.as"/>
  </applications>
  <modules/>
  <buildCSSFiles/>
  <flashCatalyst validateFlashCatalystCompatibility="false"/>
</actionScriptProperties>
      |]
    )

  , ( Right
      ( Project
      { projectSourceDirs = ["src"]
      , projectLibraryDirs = ["libs"]
      , projectLibraryFiles = []
      , projectAS3EntryPoint = "Match3.as"
      }
      , []
      )
    , [xml|
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<actionScriptProperties analytics="false" mainApplicationPath="Match3.as" projectUUID="f9cddf89-a318-422c-acf3-a0f8f677c0ce" version="10">
  <compiler additionalCompilerArguments="-locale en_US" autoRSLOrdering="true" copyDependentFiles="true" fteInMXComponents="false" generateAccessible="true" htmlExpressInstall="true" htmlGenerate="true" htmlHistoryManagement="true" htmlPlayerVersionCheck="true" includeNetmonSwc="false" outputFolderPath="bin-debug" removeUnusedRSL="true" sourceFolderPath="src" strict="true" targetPlayerVersion="0.0.0" useApolloConfig="false" useDebugRSLSwfs="true" verifyDigests="true" warn="true">
    <compilerSourcePath/>
    <libraryPath defaultLinkType="0">
      <libraryPathEntry kind="4" path="">
        <excludedEntries>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="1" linkType="1" path="${PROJECT_FRAMEWORKS}/locale/{locale}"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/advancedgrids.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/framework.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/mx/mx.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/netmon.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/sparkskins.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/rpc.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/datavisualization.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/videoPlayer.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/flash-integration.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_flashflexkit.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_agent.swc" useDefaultLinkType="false"/>
        </excludedEntries>
      </libraryPathEntry>
      <libraryPathEntry kind="1" linkType="1" path="libs"/>
    </libraryPath>
    <sourceAttachmentPath/>
  </compiler>
  <applications>
    <application path="Match3.as"/>
  </applications>
  <modules/>
  <buildCSSFiles/>
  <flashCatalyst validateFlashCatalystCompatibility="false"/>
</actionScriptProperties>
      |]
    )
  , ( Right
      ( Project
      { projectSourceDirs = ["src"]
      , projectLibraryDirs = []
      , projectLibraryFiles = ["libs/GemAssets.swc"]
      , projectAS3EntryPoint = "Match3.as"
      }
      , []
      )
    , [xml|
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<actionScriptProperties analytics="false" mainApplicationPath="Match3.as" projectUUID="9f98b2c3-1ad8-4978-92c1-40e1b732b621" version="10">
  <compiler additionalCompilerArguments="-locale en_US" autoRSLOrdering="true" copyDependentFiles="true" flexSDK="Flex 4.6.0" fteInMXComponents="false" generateAccessible="true" htmlExpressInstall="true" htmlGenerate="true" htmlHistoryManagement="true" htmlPlayerVersionCheck="true" includeNetmonSwc="false" outputFolderPath="bin-debug" removeUnusedRSL="true" sourceFolderPath="src" strict="true" targetPlayerVersion="0.0.0" useApolloConfig="false" useDebugRSLSwfs="true" verifyDigests="true" warn="true">
    <compilerSourcePath/>
    <libraryPath defaultLinkType="0">
      <libraryPathEntry kind="4" path="">
        <excludedEntries>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="1" linkType="1" path="${PROJECT_FRAMEWORKS}/locale/{locale}"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/advancedgrids.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/charts.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/framework.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/mx/mx.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/netmon.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/sparkskins.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/rpc.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/videoPlayer.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/qtp_air.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/datavisualization.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/spark_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/flash-integration.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_dmv.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_flashflexkit.swc" useDefaultLinkType="false"/>
          <libraryPathEntry kind="3" linkType="1" path="${PROJECT_FRAMEWORKS}/libs/automation_agent.swc" useDefaultLinkType="false"/>
        </excludedEntries>
      </libraryPathEntry>
      <libraryPathEntry kind="3" linkType="1" path="libs/GemAssets.swc" useDefaultLinkType="false"/>
    </libraryPath>
    <sourceAttachmentPath/>
  </compiler>
  <applications>
    <application path="Match3.as"/>
  </applications>
  <modules/>
  <buildCSSFiles/>
  <flashCatalyst validateFlashCatalystCompatibility="false"/>
</actionScriptProperties>
      |]
    )
  ]
  where
    testProject (expected, input)
      = FlashBuilder.parseDocument input ~?= expected

tests_parseCommandNames = TestList
  [ testParse []
    ""
  , testParse []
    "\n\n\n"
  , testParse ["echo"]
    "echo \"Hello world!\""
  , testParse ["echo"]
    "echo \"Hello world!\"\n"
  , testParse ["echo", "blecho"]
    "echo \"Hello world!\"\nblecho \"Goodbye world!\""
  , testParse ["C:\\path with spaces\\command.bat"]
    "\"C:\\path with spaces\\command.bat\" $(OutputDir)"
  , testParse ["debug"]
    "DEBUG:debug"
  , testParse ["debug", "release"]
    "DEBUG:debug\nRELEASE: release"
  , testParse ["Leading", "Trailing", "Both"]
    "  Leading whitespace\nTrailing whitespace   \n   Both   "
  ]
  where
    testParse expected input
      = FlashDevelop.parseCommandNames input ~?= expected
