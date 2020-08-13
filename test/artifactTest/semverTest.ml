open Format

open OUnit2

open Artifact

(* Fixtures *)

let major = 1
let minor = 2
let patch = 3
let pre_release = ["foo"; "bar"]
let build_info = ["baz"; "quux"]

let invalid_version = -1
let invalid_numeric_segment = "0123"
let invalid_alpha_segment = "abc123@!$"

let fixture = Semver.semver major minor patch [] []
let pre_release_fixture = Semver.semver major minor patch pre_release []
let build_info_fixture = Semver.semver major minor patch [] build_info
let full_meta_fixture = Semver.semver major minor patch pre_release build_info

let pre_release_segments = String.concat "." pre_release
let build_info_segments = String.concat "." build_info

let str =
  sprintf "%d.%d.%d" major minor patch
let pre_release_str =
  sprintf "%s-%s" str pre_release_segments
let build_info_str =
  sprintf "%s+%s" str build_info_segments
let full_meta_str =
  sprintf "%s-%s+%s" str pre_release_segments build_info_segments

(* Assertions *)

let assert_invalid_argument msg fn =
  let exn = Invalid_argument msg in
  assert_raises exn fn

(* Constructor *)
let test_semver =
  let test_valid =
    let assert_constructs ~ctxt expected major minor patch pre_release build_info =
      let actual = Semver.semver major minor patch pre_release build_info in
      assert_equal ~ctxt expected actual
    in

    let test_version ctxt = assert_constructs ~ctxt fixture major minor patch [] [] in
    let test_meta =
      let test_pre_release ctxt = assert_constructs ~ctxt pre_release_fixture major minor patch pre_release [] in
      let test_build_info ctxt = assert_constructs ~ctxt build_info_fixture major minor patch [] build_info in
      let test_full_meta ctxt = assert_constructs ~ctxt full_meta_fixture major minor patch pre_release build_info in
      "Metadata" >::: [
        "Pre-Release Only"           >:: test_pre_release;
        "Build Info Only"            >:: test_build_info;
        "Pre-Release and Build Info" >:: test_full_meta
      ]
    in
    "Valid" >::: [
      "Version Only" >:: test_version;
      test_meta
    ]
  in
  let test_invalid =
    let test_version =
      let assert_invalid_version name major minor patch =
        let test _ = Semver.semver major minor patch [] [] in
        let msg = sprintf "%d is not a valid %s version" invalid_version name in
        assert_invalid_argument msg test
      in

      let test_major _ = assert_invalid_version "major" invalid_version minor patch in
      let test_minor _ = assert_invalid_version "minor" major invalid_version patch in
      let test_patch _ = assert_invalid_version "patch" major minor invalid_version in
      "Version Number" >::: [
        "Major" >:: test_major;
        "Minor" >:: test_minor;
        "Patch" >:: test_patch
      ]
    in
    let test_meta =
      let assert_invalid_segment name segment pre_release build_info =
        let test _ = Semver.semver major minor patch pre_release build_info in
        let msg = sprintf "%S is not a valid %s segment" segment name in
        assert_invalid_argument msg test
      in

      let test_pre_release _ =
        assert_invalid_segment "pre-release" invalid_numeric_segment [invalid_numeric_segment] [];
        assert_invalid_segment "pre-release" invalid_alpha_segment [invalid_alpha_segment] []
      in
      let test_build_info _ =
        assert_invalid_segment "build info" invalid_numeric_segment [] [invalid_numeric_segment];
        assert_invalid_segment "build info" invalid_alpha_segment [] [invalid_alpha_segment]
      in
      "Metadata" >::: [
        "Pre-Release" >:: test_pre_release;
        "Build Info"  >:: test_build_info
      ]
    in
    "Invalid" >::: [
      test_version;
      test_meta
    ]
  in
  "Constructor" >::: [
    test_valid;
    test_invalid
  ]

(* Accessors *)
let test_accessors =
  let test_major ctxt =
    let actual = Semver.major full_meta_fixture in
    assert_equal ~ctxt major actual
  in
  let test_minor ctxt =
    let actual = Semver.minor full_meta_fixture in
    assert_equal ~ctxt minor actual
  in
  let test_patch ctxt =
    let actual = Semver.patch full_meta_fixture in
    assert_equal ~ctxt patch actual
  in
  let test_prerelease ctxt =
    let actual = Semver.prerelease full_meta_fixture in
    assert_equal ~ctxt pre_release actual
  in
  let test_build_info ctxt =
    let actual = Semver.build_info full_meta_fixture in
    assert_equal ~ctxt build_info actual
  in
  "Accessors" >::: [
    "Major Version" >:: test_major;
    "Minor Version" >:: test_minor;
    "Patch Version" >:: test_patch;
    "Pre-release"   >:: test_prerelease;
    "Build Info"    >:: test_build_info
  ]

(* Parsing *)
let test_of_string =
  let test_valid =
    let assert_parses ~ctxt expected str =
      let actual = Semver.of_string str in
      assert_equal ~ctxt expected actual
    in

    let test_version ctxt = assert_parses ~ctxt fixture str in
    let test_meta =
      let test_pre_release ctxt = assert_parses ~ctxt pre_release_fixture pre_release_str in
      let test_build_info ctxt = assert_parses ~ctxt build_info_fixture build_info_str in
      let test_full_meta ctxt = assert_parses ~ctxt full_meta_fixture full_meta_str in
      "Metadata" >::: [
        "Pre-Release Only"           >:: test_pre_release;
        "Build Info Only"            >:: test_build_info;
        "Pre-Release and Build Info" >:: test_full_meta
      ]
    in
    "Valid" >::: [
      "Version Only" >:: test_version;
      test_meta
    ]
  in
  let test_invalid =
    let assert_invalid_argument strs =
      let fn str =
        let msg = sprintf "%S is not a valid semantic version" str in
        let fn _ = Semver.of_string str in
        assert_invalid_argument msg fn
      in
      List.iter fn strs
    in
    let test_version =
      let test_missing _ =
        assert_invalid_argument [
          ".0.0"; "0..0"; "0.0.";
          "0.0";  ".0";   "0.";
          "0";
          ""
        ]
      in
      let test_major _ =
        assert_invalid_argument [
          sprintf "%s.%d.%d" invalid_numeric_segment minor patch;
          sprintf "%s.%d.%d" invalid_alpha_segment minor patch
        ]
      in
      let test_minor _ =
        assert_invalid_argument [
          sprintf "%d.%s.%d" major invalid_numeric_segment patch;
          sprintf "%d.%s.%d" major invalid_alpha_segment patch
        ]
      in
      let test_patch _ =
        assert_invalid_argument [
          sprintf "%d.%d.%s" major minor invalid_numeric_segment;
          sprintf "%d.%d.%s" major minor invalid_alpha_segment
        ]
      in
      "Version" >::: [
        "Missing" >:: test_missing;
        "Major"   >:: test_major;
        "Minor"   >:: test_minor;
        "Patch"   >:: test_patch
      ]
    in
    let test_meta =
      let test_pre_release _ =
        assert_invalid_argument [
          sprintf "%s-%s" str invalid_numeric_segment;
          sprintf "%s-%s" str invalid_alpha_segment;
          sprintf "%s-foo.%s" str invalid_numeric_segment;
          sprintf "%s-foo.%s" str invalid_alpha_segment
        ]
      in
      let test_build_info _ =
        assert_invalid_argument [
          sprintf "%s+%s" str invalid_numeric_segment;
          sprintf "%s+%s" str invalid_alpha_segment;
          sprintf "%s+foo.%s" str invalid_numeric_segment;
          sprintf "%s+foo.%s" str invalid_alpha_segment
        ]
      in
      let test_pre_release_build_info _ =
        assert_invalid_argument [
          sprintf "%s-%s+%s" str invalid_numeric_segment invalid_numeric_segment;
          sprintf "%s-%s+%s" str invalid_numeric_segment invalid_alpha_segment;
          sprintf "%s-%s+%s" str invalid_alpha_segment invalid_numeric_segment;
          sprintf "%s-%s+%s" str invalid_alpha_segment invalid_alpha_segment;
          sprintf "%s-foo.%s+%s" str invalid_numeric_segment invalid_numeric_segment;
          sprintf "%s-foo.%s+%s" str invalid_numeric_segment invalid_alpha_segment;
          sprintf "%s-%s+foo.%s" str invalid_alpha_segment invalid_numeric_segment;
          sprintf "%s-%s+foo.%s" str invalid_alpha_segment invalid_alpha_segment
        ]
      in
      "Metadata" >::: [
        "Pre-Release Only"           >:: test_pre_release;
        "Build Info Only"            >:: test_build_info;
        "Pre-Release and Build Info" >:: test_pre_release_build_info
      ]
    in
    "Invalid" >::: [
      test_version;
      test_meta
    ]
  in
  "Parsing" >::: [
    test_valid;
    test_invalid
  ]

(* Comparison *)
let test_compare =
  let assert_compare ~ctxt low high =
    let assert_compare expected x y =
      let actual = Semver.compare x y in
      assert_equal ~ctxt expected actual
    in
    assert_compare (-1) low high;
    assert_compare 1 high low;
    assert_compare 0 low low;
    assert_compare 0 high high
  in

  let test_version =
    let test_major ctxt =
      let high = Semver.semver (major + 1) minor patch [] [] in
      assert_compare ~ctxt fixture high
    in
    let test_minor ctxt =
      let high = Semver.semver major (minor + 1) patch [] [] in
      assert_compare ~ctxt fixture high
    in
    let test_patch ctxt =
      let high = Semver.semver major minor (patch + 1) [] [] in
      assert_compare ~ctxt fixture high
    in
    "Version" >::: [
      "Major" >:: test_major;
      "Minor" >:: test_minor;
      "Patch" >:: test_patch
    ]
  in
  let test_meta =
    let test_pre_release ctxt =
      assert_compare ~ctxt pre_release_fixture fixture;

      let high = Semver.semver major minor patch ["1"] [] in
      assert_compare ~ctxt pre_release_fixture high;

      let low = Semver.semver major minor patch ["a"] [] in
      assert_compare ~ctxt low pre_release_fixture;

      let pre_release = pre_release @ ["a"] in
      let low = Semver.semver major minor patch pre_release [] in
      assert_compare ~ctxt low pre_release_fixture
    in
    let test_build_info ctxt =
      let assert_compare x y =
        let actual = Semver.compare x y in
        assert_equal ~ctxt 0 actual
      in

      assert_compare build_info_fixture fixture;

      let high = Semver.semver major minor patch [] ["1"] in
      assert_compare build_info_fixture high;

      let low = Semver.semver major minor patch [] ["a"] in
      assert_compare low build_info_fixture;

      let build_info = build_info @ ["a"] in
      let low = Semver.semver major minor patch [] build_info in
      assert_compare low build_info_fixture
    in
    "Meta" >::: [
      "Pre-Release" >:: test_pre_release;
      "Build Info"  >:: test_build_info
    ]
  in
  "Comparison" >::: [
    test_version;
    test_meta
  ]

(* Formatting *)
let test_format =
  let assert_formats ~ctxt expected semver =
    Semver.format str_formatter semver;
    let actual = flush_str_formatter () in
    assert_equal ~ctxt expected actual
  in

  let test_version ctxt = assert_formats ~ctxt str fixture in
  let test_meta =
    let test_pre_release ctxt = assert_formats ~ctxt pre_release_str pre_release_fixture in
    let test_build_info ctxt = assert_formats ~ctxt build_info_str build_info_fixture in
    let test_full_meta ctxt = assert_formats ~ctxt full_meta_str full_meta_fixture in
    "Metadata" >::: [
      "Pre-Release Only"           >:: test_pre_release;
      "Build Info Only"            >:: test_build_info;
      "Pre-Release and Build Info" >:: test_full_meta
    ]
  in
  "Formatting" >::: [
    "Version Only" >:: test_version;
    test_meta
  ]

(* Printing *)
let test_to_string =
  let assert_string ~ctxt expected semver =
    let actual = Semver.to_string semver in
    assert_equal ~ctxt expected actual
  in

  let test_version ctxt = assert_string ~ctxt str fixture in
  let test_meta =
    let test_pre_release ctxt = assert_string ~ctxt pre_release_str pre_release_fixture in
    let test_build_info ctxt = assert_string ~ctxt build_info_str build_info_fixture in
    let test_full_meta ctxt = assert_string ~ctxt full_meta_str full_meta_fixture in
    "Metadata" >::: [
      "Pre-Release Only"           >:: test_pre_release;
      "Build Info Only"            >:: test_build_info;
      "Pre-Release and Build Info" >:: test_full_meta
    ]
  in
  "Printing" >::: [
    "Version Only" >:: test_version;
    test_meta
  ]

(* Compatibility Groups *)
let test_compatibility_groups =
  let incompatible_major_version = Semver.semver (major + 1) minor patch [] [] in
  let compatible_minor_version = Semver.semver major (minor + 1) patch [] [] in
  let compatible_minor_prerelease_version = Semver.semver major (minor + 1) patch ["alpha"] [] in
  let compatible_patch_version = Semver.semver major minor (patch + 1) [] [] in

  let incompatible_major_value = "incompatible-major" in
  let compatible_minor_value = "compatible-minor" in
  let compatible_minor_prerelease_value = "compatible-minor-prerelease" in
  let compatible_patch_value = "compatible-patch" in

  let incompatible_major = (incompatible_major_version, incompatible_major_value) in
  let compatible_minor = (compatible_minor_version, compatible_minor_value) in
  let compatible_minor_prerelease = (compatible_minor_prerelease_version, compatible_minor_prerelease_value) in
  let compatible_patch = (compatible_patch_version, compatible_patch_value) in

  let add (semver, value) acc = Semver.add semver value acc in

  let test_find =
    let grp = Semver.add compatible_minor_version compatible_minor_value Semver.empty in

    let test_found ctxt =
      let actual = Semver.find compatible_minor_version grp in
      assert_equal ~ctxt compatible_minor_value actual
    in
    let test_not_found _ =
      let fn _ = Semver.find incompatible_major_version grp in
      let exn = Not_found in
      assert_raises exn fn
    in
    let test_empty _ =
      let fn _ = Semver.find compatible_minor_version Semver.empty in
      let exn = Not_found in
      assert_raises exn fn
    in
    "Find" >::: [
      "Found"     >:: test_found;
      "Not Found" >:: test_not_found;
      "Empty"     >:: test_empty
    ]
  in
  let test_latest =
    let test_empty _ =
      let grp = List.fold_right add [incompatible_major] Semver.empty in
      let fn _ = Semver.latest major grp in
      assert_raises Not_found fn
    in
    let test_only_prerelease _ =
      let grp = List.fold_right add [incompatible_major; compatible_minor_prerelease] Semver.empty in
      let fn _ = Semver.latest major grp in
      assert_raises Not_found fn
    in
    let test_only ctxt =
      let grp = List.fold_right add [incompatible_major; compatible_minor] Semver.empty in
      Semver.latest major grp
        |> assert_equal ~ctxt compatible_minor_version
    in
    let test_newest ctxt =
      let grp = List.fold_right add [incompatible_major; compatible_minor; compatible_patch] Semver.empty in
      Semver.latest major grp
        |> assert_equal ~ctxt compatible_minor_version
    in
    let test_ignores_prerelease ctxt =
      let grp = List.fold_right add [incompatible_major; compatible_minor_prerelease; compatible_patch] Semver.empty in
      Semver.latest major grp
        |> assert_equal ~ctxt compatible_patch_version
    in
    "Latest (Released)" >::: [
      "Empty"                >:: test_empty;
      "Only Pre-release"     >:: test_only_prerelease;
      "Only Version"         >:: test_only;
      "Newest Version"       >:: test_newest;
      "Ignores Pre-releases" >:: test_ignores_prerelease
    ]
  in
  let test_latest_prerelease =
    let test_empty _ =
      let grp = List.fold_right add [incompatible_major] Semver.empty in
      let fn _ = Semver.latest_prerelease major grp in
      assert_raises Not_found fn
    in
    let test_only ctxt =
      let grp = List.fold_right add [incompatible_major; compatible_minor_prerelease] Semver.empty in
      Semver.latest_prerelease major grp
        |> assert_equal ~ctxt compatible_minor_prerelease_version
    in
    let test_newest ctxt =
      let grp = List.fold_right add [incompatible_major; compatible_minor; compatible_minor_prerelease; compatible_patch] Semver.empty in
      Semver.latest_prerelease major grp
        |> assert_equal ~ctxt compatible_minor_version
    in
    let test_includes_prerelease ctxt =
      let grp = List.fold_right add [incompatible_major; compatible_minor_prerelease; compatible_patch] Semver.empty in
      Semver.latest_prerelease major grp
        |> assert_equal ~ctxt compatible_minor_prerelease_version
    in
    "Latest Prerelease" >::: [
      "Empty"                 >:: test_empty;
      "Only Version"          >:: test_only;
      "Newest Version"        >:: test_newest;
      "Includes Pre-releases" >:: test_includes_prerelease
    ]
  in
  "Compatibility Groups" >::: [
    test_find;
    test_latest;
    test_latest_prerelease
  ]

(* Test Suite *)
let suite =
  "Semantic Versions" >::: [
    test_semver;
    test_accessors;
    test_of_string;
    test_compare;
    test_format;
    test_to_string;
    test_compatibility_groups
  ]
