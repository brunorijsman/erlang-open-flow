%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman

-module(of_cover).

-export([coverage_dir/2]).

coverage_dir(SourceCodeDir, OutputDir) ->
    delete_html_files(OutputDir),
    {ok, ErrorFiles, Modules} = instrument(SourceCodeDir),
    CoverageResults = lists:map(fun coverage_module/1, Modules),
    generate_html(OutputDir, ErrorFiles, CoverageResults).

coverage_module(Module) ->
    case erlang:function_exported(Module, test, 0) of
        true -> 
            coverage_module_with_test(Module);
        false -> 
            coverage_module_without_test(Module)
    end.

coverage_module_without_test(Module) ->
    {Module, "Module does not have unit test", undefined}.

coverage_module_with_test(Module) ->
    case Module:test() of
        ok ->
            coverage_module_with_passed_test(Module);
        error ->
            coverage_module_with_failed_test(Module)
    end.

coverage_module_with_failed_test(Module) ->
    {Module, "Module failed unit test", undefined}.

coverage_module_with_passed_test(Module) ->
    {ok, {Module, {Cov, NotCov}}} = cover:analyse(Module, module),
    Total = Cov + NotCov,
    Result = case Total of
                 0 -> "No code in module";
                 _ -> io_lib:format("~w %", [round(100.0 * Cov / Total)])
             end,
    {ok, HtmlFile} = cover:analyse_to_file(Module, [html]),
    {Module, Result, HtmlFile}.

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

generate_html(OutputDir, ErrorFiles, CoverageResults) ->
    FileName = OutputDir ++ "/summary.html",
    {ok, IoDevice} = file:open(FileName, [write]),
    ok = io:format(IoDevice, "<html>~n", []),
    ok = generate_html_head(IoDevice),
    ok = generate_html_body(IoDevice, ErrorFiles, CoverageResults),
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

generate_html_body(IoDevice, ErrorFiles, CoverageResults) ->
    ok = io:format(IoDevice, "<body>~n", []),
    ok = io:format(IoDevice, "<h1>Openflow unit-test coverage summary</h1>~n", []),
    ok = generate_html_results_table(IoDevice, CoverageResults),
    case ErrorFiles of
        [] -> ok;
        _  -> ok = generate_html_compile_errors_table(IoDevice, ErrorFiles)
    end,
    ok = io:format(IoDevice, "</body>~n", []),
    ok.
    
generate_html_results_table(IoDevice, CoverageResults) ->
    ok = io:format(IoDevice, "<h2>Results</h2>~n", []),
    ok = io:format(IoDevice, "<table>~n", []),
    ok = io:format(IoDevice, "<tr>~n", []),
    ok = io:format(IoDevice, "<th>Module</th>~n", []),
    ok = io:format(IoDevice, "<th>Coverage</th>~n", []),
    ok = io:format(IoDevice, "<th>Details</th>~n", []),
    ok = io:format(IoDevice, "</tr>~n", []),
    lists:foreach(fun(Result) -> 
                          generate_html_results_table_row(IoDevice, Result) 
                  end, 
                  CoverageResults),
    ok = io:format(IoDevice, "</table>~n", []),
    ok.

generate_html_results_table_row(IoDevice, {Module, Result, DetailFile}) ->
    ok = io:format(IoDevice, "<tr>~n", []),
    ok = io:format(IoDevice, "<td>~w</td>~n", [Module]),
    ok = io:format(IoDevice, "<td>~s</td>~n", [Result]),
    case DetailFile of
        undefined ->
            ok = io:format(IoDevice, "<td>not available</td>~n", []);
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

