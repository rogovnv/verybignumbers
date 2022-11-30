%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. окт. 2022 19:23
%%%-------------------------------------------------------------------
{application, base_calc, [
  {description, "Basic-like calculator with conds&loops, using via HTTP"},
  {vsn, "1.0"},
  {registered, [g_repo, th_sup, conn_sup, top_sup, gr_sup, left_one_sup, right_all_sup]},
  {applications, [
    sasl,
    kernel,
    stdlib
  ]},
  {mod, {base_calc, []}},
  {env, []}
]}.