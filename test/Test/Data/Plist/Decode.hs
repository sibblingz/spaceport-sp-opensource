{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Plist.Decode where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

import qualified Data.ByteString as BS

import Data.Plist.Decode.Binary
import Data.Plist.Decode.XML

import QuasiQuoters

main = defaultMain tests
tests = [$(testGroupGenerator)]
  ++ hUnitTestToTests (TestLabel "binaryParsedEqualsXMLParsed" tests_binaryParsedEqualsXMLParsed)
  ++ hUnitTestToTests (TestLabel "readIntegerSignedBE" tests_readIntegerSignedBE)

tests_readIntegerSignedBE = TestList $ map testRead
  [ ([0x00], 0)
  , ([0x20], 32)
  , ([0x7F], 127)
  , ([0x80], -128)
  , ([0x81], -127)
  , ([0xC0], -64)
  , ([0xFE], -2)
  , ([0xFF], -1)
  ]
  where
    testRead (binary, expected)
      = readIntegerSignedBE (BS.pack binary) ~?= expected

tests_binaryParsedEqualsXMLParsed = TestList $ map testParse
  [ ( "bplist00\223\DLE\"\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US !\"#&'(-./010346789:;<=>00ADEFJKLMNOP_\DLE\FSUIRequiredDeviceCapabilitiesZDTCompiler_\DLE\GSCFBundleInfoDictionaryVersion_\DLE%UISupportedInterfaceOrientations~ipad_\DLE\DC1DTPlatformVersion\\CFBundleNameYDTSDKName_\DLE\DC2LSRequiresIPhoneOS_\DLE\DC3CFBundleDisplayName_\DLE\SYNUIStatusBarHidden~ipadZDTSDKBuild_\DLE\SUBCFBundleSupportedPlatforms_\DLE UISupportedInterfaceOrientations_\DLE\DC3BuildMachineOSBuild_\DLE\SIDTPlatformBuild_\DLE\DLEMinimumOSVersion\\DTXcodeBuild_\DLE\EMCFBundleDevelopmentRegion_\DLE\DC3CFBundlePackageType_\DLE\SICFBundleVersion_\DLE\DC1UIPrerenderedIcon_\DLE\DC1UIStatusBarHidden_\DLE\ESCUIApplicationExitsOnSuspend^UIDeviceFamily]NSMainNibFile_\DLE\DC2CFBundleIdentifier_\DLE\DC1CFBundleIconFilesWDTXcode_\DLE\DC2CFBundleExecutable_\DLE\DC2NSMainNibFile~ipad_\DLE\DC1CFBundleSignature^DTPlatformName_\DLE\GSCFBundleResourceSpecification_\DLE\DLECFBundleURLTypes\162$%Zopengles-2Uarmv7_\DLE\"com.apple.compilers.llvm.clang.1_0S6.0\164)*+,_\DLE#UIInterfaceOrientationLandscapeLeft_\DLE$UIInterfaceOrientationLandscapeRight_\DLE\RSUIInterfaceOrientationPortrait_\DLE(UIInterfaceOrientationPortraitUpsideDownS5.1_\DLE\DC1spaceportappnolib[iphoneos5.1\tYSpaceport\tU9B176\161\&5XiPhoneOS\164)*,+U11E53U9B176S4.3V4E3002WEnglishTAPPLU4.0.0\b\t\t\162BC\DLE\SOH\DLE\STXZMainWindow_\DLE\SUBcom.sibblingz.spaceportapp\163GHI_\DLE\SYNSpaceportApp_57x57.png_\DLE\CANSpaceportApp_114x114.png_\DLE\SYNSpaceportApp_72x72.pngT0433_\DLE\DC1spaceportappnolib_\DLE\SIMainWindow-iPadT????Xiphoneos_\DLE\DC3ResourceRules.plist\161Q\211RSTUVW_\DLE\DLECFBundleTypeRole_\DLE\SICFBundleURLName_\DLE\DC2CFBundleURLSchemesVViewer_\DLE\ETBcom.sibblingz.spaceport\162XY_\DLE\DC1fb235939739802108Yspaceport\NUL\b\NULO\NULn\NULy\NUL\153\NUL\193\NUL\213\NUL\226\NUL\236\SOH\SOH\SOH\ETB\SOH0\SOH;\SOHX\SOH{\SOH\145\SOH\163\SOH\182\SOH\195\SOH\223\SOH\245\STX\a\STX\ESC\STX/\STXM\STX\\\STXj\STX\DEL\STX\147\STX\155\STX\176\STX\197\STX\217\STX\232\ETX\b\ETX\ESC\ETX\RS\ETX)\ETX/\ETXT\ETXX\ETX]\ETX\131\ETX\170\ETX\203\ETX\246\ETX\250\EOT\SO\EOT\SUB\EOT\ESC\EOT%\EOT&\EOT,\EOT.\EOT7\EOT<\EOTB\EOTH\EOTL\EOTS\EOT[\EOT`\EOTf\EOTg\EOTh\EOTi\EOTl\EOTn\EOTp\EOT{\EOT\152\EOT\156\EOT\181\EOT\208\EOT\233\EOT\238\ENQ\STX\ENQ\DC4\ENQ\EM\ENQ\"\ENQ8\ENQ:\ENQA\ENQT\ENQf\ENQ{\ENQ\130\ENQ\156\ENQ\159\ENQ\179\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NULZ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\189"
    , [xml|
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>BuildMachineOSBuild</key>
	<string>11E53</string>
	<key>CFBundleDevelopmentRegion</key>
	<string>English</string>
	<key>CFBundleDisplayName</key>
	<string>Spaceport</string>
	<key>CFBundleExecutable</key>
	<string>spaceportappnolib</string>
	<key>CFBundleIconFiles</key>
	<array>
		<string>SpaceportApp_57x57.png</string>
		<string>SpaceportApp_114x114.png</string>
		<string>SpaceportApp_72x72.png</string>
	</array>
	<key>CFBundleIdentifier</key>
	<string>com.sibblingz.spaceportapp</string>
	<key>CFBundleInfoDictionaryVersion</key>
	<string>6.0</string>
	<key>CFBundleName</key>
	<string>spaceportappnolib</string>
	<key>CFBundlePackageType</key>
	<string>APPL</string>
	<key>CFBundleResourceSpecification</key>
	<string>ResourceRules.plist</string>
	<key>CFBundleSignature</key>
	<string>????</string>
	<key>CFBundleSupportedPlatforms</key>
	<array>
		<string>iPhoneOS</string>
	</array>
	<key>CFBundleURLTypes</key>
	<array>
		<dict>
			<key>CFBundleTypeRole</key>
			<string>Viewer</string>
			<key>CFBundleURLName</key>
			<string>com.sibblingz.spaceport</string>
			<key>CFBundleURLSchemes</key>
			<array>
				<string>fb235939739802108</string>
				<string>spaceport</string>
			</array>
		</dict>
	</array>
	<key>CFBundleVersion</key>
	<string>4.0.0</string>
	<key>DTCompiler</key>
	<string>com.apple.compilers.llvm.clang.1_0</string>
	<key>DTPlatformBuild</key>
	<string>9B176</string>
	<key>DTPlatformName</key>
	<string>iphoneos</string>
	<key>DTPlatformVersion</key>
	<string>5.1</string>
	<key>DTSDKBuild</key>
	<string>9B176</string>
	<key>DTSDKName</key>
	<string>iphoneos5.1</string>
	<key>DTXcode</key>
	<string>0433</string>
	<key>DTXcodeBuild</key>
	<string>4E3002</string>
	<key>LSRequiresIPhoneOS</key>
	<true/>
	<key>MinimumOSVersion</key>
	<string>4.3</string>
	<key>NSMainNibFile</key>
	<string>MainWindow</string>
	<key>NSMainNibFile~ipad</key>
	<string>MainWindow-iPad</string>
	<key>UIApplicationExitsOnSuspend</key>
	<true/>
	<key>UIDeviceFamily</key>
	<array>
		<integer>1</integer>
		<integer>2</integer>
	</array>
	<key>UIPrerenderedIcon</key>
	<false/>
	<key>UIRequiredDeviceCapabilities</key>
	<array>
		<string>opengles-2</string>
		<string>armv7</string>
	</array>
	<key>UIStatusBarHidden</key>
	<true/>
	<key>UIStatusBarHidden~ipad</key>
	<true/>
	<key>UISupportedInterfaceOrientations</key>
	<array>
		<string>UIInterfaceOrientationLandscapeLeft</string>
		<string>UIInterfaceOrientationLandscapeRight</string>
		<string>UIInterfaceOrientationPortraitUpsideDown</string>
		<string>UIInterfaceOrientationPortrait</string>
	</array>
	<key>UISupportedInterfaceOrientations~ipad</key>
	<array>
		<string>UIInterfaceOrientationLandscapeLeft</string>
		<string>UIInterfaceOrientationLandscapeRight</string>
		<string>UIInterfaceOrientationPortrait</string>
		<string>UIInterfaceOrientationPortraitUpsideDown</string>
	</array>
</dict>
</plist>
      |]
    )
  ]
  where
    testParse (binary, xmlData)
      = fromBinary binary ~?= fromXMLDocument xmlData
