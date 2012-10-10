%%%=====================================================================================================================
%%% Copyright (c) 2012, Bruno Rijsman
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted provided that the 
%%% following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this list of conditions and the following 
%%%   disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the 
%%%   following disclaimer in the documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
%%% USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%=====================================================================================================================

%%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%%% @copyright 2012 Bruno Rijsman

%% TODO: Add type checking
%% TODO: Add support for 'excempt files' (e.g. of_cover itself)
%% TODO: Add total coverage at bottom of table
%% TODO: Report when generated
%% TODO: Report unit test details (error messages, run time, etc.)
%% TODO: Add color coding (green for pass, red for fail)
%% TODO: Coverage counts are statements, not lines
%% TODO: Order results alphabetically by module name

-module(of_cover).

-export([coverage_dir/2]).

-record(test_result, {
          module,
          test_result}).
          
-record(coverage_result, {
          module,
          lines_covered,
          lines_not_covered,
          coverage_html_file}).

-record(combined_result, {
          module,
          test_result,
          lines_covered,
          lines_not_covered,
          coverage_html_file}).

coverage_dir(SourceCodeDir, OutputDir) ->
    delete_html_files(OutputDir),
    {ok, ErrorFiles, Modules} = instrument(SourceCodeDir),
    TestResults = lists:map(fun test_module/1, Modules),
    CoverageResults = lists:map(fun coverage_module/1, Modules),
    CombinedResults = combine_results(Modules, TestResults, CoverageResults),
    generate_html(OutputDir, ErrorFiles, CombinedResults).

delete_html_files(OutputDir) ->
    {ok, AllFiles} = file:list_dir(OutputDir),
    HtmlFiles = lists:filter(fun is_html_file/1, AllFiles),
    lists:foreach(fun file:delete/1, HtmlFiles).
   
is_html_file(FileName) ->
    file_extension(FileName) == "html".

file_extension(FileName) ->
    case string:tokens(FileName, ".") of
        [] ->
            "";
        Parts ->
            lists:last(Parts)
    end.

instrument(SourceCodeDir) ->
    CompileResult = cover:compile_directory(SourceCodeDir),
    ErrorFiles = [ File || {error, File} <- CompileResult ],
    Modules = [Module || {ok, Module} <- CompileResult ],
    {ok, ErrorFiles, Modules}.

test_module(Module) ->
    case erlang:function_exported(Module, test, 0) of
        true -> 
            test_module_with_test(Module);
        false -> 
            test_module_without_test(Module)
    end.

test_module_without_test(Module) ->
    #test_result{module = Module, test_result = "Module does not have unit test"}.

test_module_with_test(Module) ->
    case Module:test() of
        ok ->
            #test_result{module = Module, test_result = "Pass"};
        error ->
            #test_result{module = Module, test_result = "Fail"}
    end.

coverage_module(Module) ->
    {ok, {Module, {LinesCovered, LinesNotCovered}}} = cover:analyse(Module, module),
    {ok, CoverageHtmlFile} = cover:analyse_to_file(Module, [html]),
    #coverage_result{module = Module, 
                     lines_covered = LinesCovered, 
                     lines_not_covered = LinesNotCovered,
                     coverage_html_file = CoverageHtmlFile}.

combine_results(Modules, TestResults, CoverageResults) ->
    lists:map(fun (Module) ->
                      combine_module_results(Module, 
                                             TestResults, 
                                             CoverageResults)
              end, 
              Modules).

combine_module_results(Module, TestResults, CoverageResults) ->
    TestResult = lists:keyfind(Module, #test_result.module, TestResults),
    CoverageResult = lists:keyfind(Module, #coverage_result.module, CoverageResults),
    #combined_result{module             = Module,
                     test_result        = TestResult#test_result.test_result,
                     lines_covered      = CoverageResult#coverage_result.lines_covered,
                     lines_not_covered  = CoverageResult#coverage_result.lines_not_covered,
                     coverage_html_file = CoverageResult#coverage_result.coverage_html_file}.

percent_coverage(LinesCovered, LinesNotCovered) ->
    TotalLines = LinesCovered + LinesNotCovered,
    case TotalLines of
        0 -> "-";
        _ -> io_lib:format("~w %", [round(100.0 * LinesCovered / TotalLines)])
    end.

generate_html(OutputDir, ErrorFiles, CombinedResults) ->
    FileName = OutputDir ++ "/summary.html",
    {ok, IoDevice} = file:open(FileName, [write]),
    ok = io:format(IoDevice, "<html>~n", []),
    ok = generate_html_head(IoDevice),
    ok = generate_html_body(IoDevice, ErrorFiles, CombinedResults),
    ok = io:format(IoDevice, "</html>~n", []),
    ok = file:close(IoDevice),
    ok.

generate_html_head(IoDevice) ->
    ok = io:format(IoDevice, "<head>~n", []),
    ok = generate_html_title(IoDevice),
    ok = generate_html_css_link(IoDevice),
    ok = io:format(IoDevice, "</head>~n", []),
    ok.

generate_html_title(IoDevice) ->
    ok = io:format(IoDevice, "<title>", []),
    ok = io:format(IoDevice, "Openflow unit-test coverage summary", []),
    ok = io:format(IoDevice, "</title>~n", []),
    ok.

generate_html_css_link(IoDevice) ->
    ok = io:format(IoDevice, 
                   "<link type=\"text/css\" "
                   "rel=\"stylesheet\" "
                   "href=\"style.css\" />~n", []),
    ok.

generate_html_body(IoDevice, ErrorFiles, CombinedResults) ->
    ok = io:format(IoDevice, "<body>~n", []),
    ok = io:format(IoDevice, "<h1>Openflow unit-test summary</h1>~n", []),
    ok = generate_html_results_table(IoDevice, CombinedResults),
    case ErrorFiles of
        [] -> ok;
        _  -> ok = generate_html_compile_errors_table(IoDevice, ErrorFiles)
    end,
    ok = io:format(IoDevice, "</body>~n", []),
    ok.
    
generate_html_results_table(IoDevice, CombinedResults) ->
    ok = io:format(IoDevice, "<h2>Results</h2>~n", []),
    ok = io:format(IoDevice, "<table>~n", []),
    ok = io:format(IoDevice, "<tr>~n", []),
    ok = io:format(IoDevice, "<th>Module</th>~n", []),
    ok = io:format(IoDevice, "<th>Unit test result</th>~n", []),
    ok = io:format(IoDevice, "<th>Lines covered</th>~n", []),
    ok = io:format(IoDevice, "<th>Lines not covered</th>~n", []),
    ok = io:format(IoDevice, "<th>Coverage percentage</th>~n", []),
    ok = io:format(IoDevice, "<th>Coverage details</th>~n", []),
    ok = io:format(IoDevice, "</tr>~n", []),
    lists:foreach(fun(CoverageResult) -> 
                          generate_html_results_table_row(IoDevice, CoverageResult) 
                  end, 
                  CombinedResults),
    ok = io:format(IoDevice, "</table>~n", []),
    ok.

generate_html_results_table_row(IoDevice, CombinedResult) ->
    #combined_result{module             = Module,
                     test_result        = TestResult,
                     lines_covered      = LinesCovered,
                     lines_not_covered  = LinesNotCovered,
                     coverage_html_file = CoverageHtmlFile} = CombinedResult,
    PercentCoverage = percent_coverage(LinesCovered, LinesNotCovered),
    ok = io:format(IoDevice, "<tr>~n", []),
    ok = io:format(IoDevice, "<td>~w</td>~n", [Module]),
    ok = io:format(IoDevice, "<td>~s</td>~n", [TestResult]),
    ok = io:format(IoDevice, "<td>~w</td>~n", [LinesCovered]),
    ok = io:format(IoDevice, "<td>~w</td>~n", [LinesNotCovered]),
    ok = io:format(IoDevice, "<td>~s</td>~n", [PercentCoverage]),
    case CoverageHtmlFile of
        undefined ->
            ok = io:format(IoDevice, "<td>Not available</td>~n", []);
        _ ->
            ok = io:format(IoDevice, "<td><a href=\"~s.COVER.html\">here</a></td>", 
                           [Module])
    end,
    ok = io:format(IoDevice, "</tr>~n", []),
    ok.

generate_html_compile_errors_table(IoDevice, ErrorFiles) ->
    ok = io:format(IoDevice, "<h2>Compile errors</h2>~n", []),
    ok = io:format(IoDevice, "<table>~n", []),
    ok = io:format(IoDevice, "<tr>~n", []),
    ok = io:format(IoDevice, "<th>File</th>~n", []),
    ok = io:format(IoDevice, "</tr>~n", []),
    lists:foreach(fun(FileName) -> 
                          generate_html_compile_errors_table_row(IoDevice, FileName)
                  end,
                  ErrorFiles),
    ok = io:format(IoDevice, "</table>~n", []),
    ok.

generate_html_compile_errors_table_row(IoDevice, FileName) ->
    ok = io:format(IoDevice, "<tr>~n", []),
    ok = io:format(IoDevice, "<td>~s</td>~n", [FileName]),
    ok = io:format(IoDevice, "</tr>~n", []),
    ok.

