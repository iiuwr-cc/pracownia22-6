open Zi_lib.Iface
open Zi_lib.Plugin
open Zi_lib.Plugin_register

module LexerAndParser = struct
  type token = Parser.token

  module Lexer = Lexer
  module Parser = Parser
end

module Plugin : PLUGIN = struct
  let version = "na"
  let make_live_variables_analysis = None
  let make_dominators_analysis = None
  let make_scheduler = None
  let make_natural_loops_analysis = None
  let make_spill_costs_analysis = None
  let lexer_and_parser = None
  let make_interface_checker = None
  let make_typechecker = None
  let make_translator = None
  let make_jump_threading = None
  let make_hilower = None
  let make_callconv = None
  let make_mipslower = None
  let make_register_allocator = None
  let make_register_coalescing = None
  let make_constant_folding_analysis = 
    Some (module Constant_folding_analysis.Make : MAKE_CONSTANT_FOLDING_ANALYSIS)
  let make_constant_folding = 
    Some (module Constant_folding.Make : MAKE_CONSTANT_FOLDING)
  let make_codegen = None
  let make_dead_code_elimination = None
  let make_interference_graph_analysis = None
  let make_spilling = None
  let make_reachability_analysis = None
end

module RegisterMyPlugin = RegisterPlugin (Plugin)
