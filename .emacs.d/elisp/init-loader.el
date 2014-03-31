<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  
  

  
<head>
    <title>
      /lang/elisp/init-loader/init-loader.el –
      CodeRepos::Share – Trac
    </title>
        <link rel="search" href="/share/search" />
        <link rel="help" href="/share/wiki/TracGuide" />
        <link rel="alternate" href="/share/browser/lang/elisp/init-loader/init-loader.el?format=txt" type="text/plain" title="Plain Text" /><link rel="alternate" href="/share/export/39278/lang/elisp/init-loader/init-loader.el" type="text/x-elisp; charset=utf-8" title="Original Format" />
        <link rel="up" href="/share/browser/lang/elisp/init-loader" title="Parent directory" />
        <link rel="start" href="/share/wiki" />
        <link rel="stylesheet" href="/share/chrome/common/css/trac.css" type="text/css" /><link rel="stylesheet" href="/share/chrome/common/css/code.css" type="text/css" /><link rel="stylesheet" href="/share/chrome/common/css/browser.css" type="text/css" />
        <link rel="shortcut icon" href="/share/chrome/common/trac.ico" type="image/x-icon" />
        <link rel="icon" href="/share/chrome/common/trac.ico" type="image/x-icon" />
      <link type="application/opensearchdescription+xml" rel="search" href="/share/search/opensearch" title="Search CodeRepos::Share" />
    <script type="text/javascript" src="/share/chrome/common/js/jquery.js"></script><script type="text/javascript" src="/share/chrome/common/js/trac.js"></script><script type="text/javascript" src="/share/chrome/common/js/search.js"></script>
    <!--[if lt IE 7]>
    <script type="text/javascript" src="/share/chrome/common/js/ie_pre7_hacks.js"></script>
    <![endif]-->
    <link rel="stylesheet" type="text/css" media="screen,tv,projection" title="ass-ari" href="http://svn.coderepos.org/share/websites/coderepos.org/trac/share/styles/ass-ari.css" /><link rel="alternate stylesheet" type="text/css" media="screen,tv,projection" title="plants-repository" href="http://svn.coderepos.org/share/websites/coderepos.org/trac/share/styles/plants-repository.css" /><link rel="alternate stylesheet" type="text/css" media="screen,tv,projection" title="default" href="http://svn.coderepos.org/share/websites/coderepos.org/trac/share/styles/default.css" /><link rel="openid2.provider" href="http://openid.coderepos.org/auth" /><link rel="openid.server" href="http://openid.coderepos.org/auth" /><script type="text/javascript" src="http://svn.coderepos.org/share/lang/javascript/javascript-xpath/trunk/release/javascript-xpath-latest-cmp.js"></script><script type="text/javascript" src="http://svn.coderepos.org/share/lang/javascript/javascript-xpath/bindings/jquery/src/xpath4jquery.js"></script><script type="text/javascript" src="http://svn.coderepos.org/share/websites/coderepos.org/trac/share/js/TracUtils.js"></script>
  </head>


  

  <head>
    <title>/lang/elisp/init-loader/init-loader.el</title>
    <script type="text/javascript">
      jQuery(document).ready(function($) {
        $("#jumploc input").hide();
        $("#jumploc select").change(function () {
          this.parentNode.parentNode.submit();
        })
      });
    </script>
  </head>
  <body>
    <div id="banner">
      <div id="header">
        <a id="logo" href="/share/wiki/TracIni#header_logo-section"><img src="/share/chrome/site/your_project_logo.png" alt="(please configure the [header_logo] section in trac.ini)" /></a>
      </div>
      <form id="search" action="/share/search" method="get">
        <div>
          <label for="proj-search">Search:</label>
          <input type="text" id="proj-search" name="q" size="18" value="" />
          <input type="submit" value="Search" />
        </div>
      </form>
      <div id="metanav" class="nav">
    <ul>
      <li class="first"><a href="/share/login">Login</a></li><li><a href="/share/prefs">Preferences</a></li><li><a href="/share/wiki/TracGuide">Help/Guide</a></li><li class="last"><a href="/share/about">About Trac</a></li>
    </ul>
  </div>
    </div>
    <div id="mainnav" class="nav">
    <ul>
      <li class="first"><a href="/share/wiki">Wiki</a></li><li><a href="/share/timeline">Timeline</a></li><li><a href="/share/roadmap">Roadmap</a></li><li class="active"><a href="/share/browser">Browse Source</a></li><li><a href="/share/report">View Tickets</a></li><li class="last"><a href="/share/search">Search</a></li>
    </ul>
  </div>
    <div id="main">
      <div id="ctxtnav" class="nav">
        <h2>Context Navigation</h2>
          <ul>
            <li class="first "><a href="/share/changeset/28647/lang/elisp/init-loader/init-loader.el">Last Change</a></li><li><a href="/share/browser/lang/elisp/init-loader/init-loader.el?annotate=blame&amp;rev=28647" title="Annotate each line with the last changed revision (this can be time consuming...)">Annotate</a></li><li class="last"><a href="/share/log/lang/elisp/init-loader/init-loader.el">Revision Log</a></li>
          </ul>
        <hr />
      </div>
    <div id="content" class="browser">
      <h1>
    <a class="pathentry first" title="Go to root directory" href="/share/browser">root</a><span class="pathentry sep">/</span><a class="pathentry" title="View lang" href="/share/browser/lang">lang</a><span class="pathentry sep">/</span><a class="pathentry" title="View elisp" href="/share/browser/lang/elisp">elisp</a><span class="pathentry sep">/</span><a class="pathentry" title="View init-loader" href="/share/browser/lang/elisp/init-loader">init-loader</a><span class="pathentry sep">/</span><a class="pathentry" title="View init-loader.el" href="/share/browser/lang/elisp/init-loader/init-loader.el">init-loader.el</a>
    <br style="clear: both" />
  </h1>
      <div id="jumprev">
        <form action="" method="get">
          <div>
            <label for="rev">
              View revision:</label>
            <input type="text" id="rev" name="rev" size="6" />
          </div>
        </form>
      </div>
      <table id="info" summary="Revision info">
        <tr>
          <th scope="col">
            Revision <a href="/share/changeset/28647">28647</a>, <span title="7987 bytes">7.8 kB</span>
            (checked in by imakado, <a class="timeline" href="/share/timeline?from=2009-01-19T11%3A38%3A33Z%2B0900&amp;precision=second" title="2009-01-19T11:38:33Z+0900 in Timeline">5 years</a> ago)
          </th>
        </tr>
        <tr>
          <td class="message searchable">
              <p>
バイトコンパイルするとうまく動かない可能性があったので,cl関数のfirstをcarに置き換えた <br />
</p>
          </td>
        </tr>
      </table>
      <div id="preview" class="searchable">
    <table class="code"><thead><tr><th class="lineno" title="Line numbers">Line</th><th class="content"> </th></tr></thead><tbody><tr><th id="L1"><a href="#L1">1</a></th><td>;;;  -*- coding: utf-8; mode: emacs-lisp; -*-</td></tr><tr><th id="L2"><a href="#L2">2</a></th><td>;;; init-loader.el ---</td></tr><tr><th id="L3"><a href="#L3">3</a></th><td></td></tr><tr><th id="L4"><a href="#L4">4</a></th><td>;; Author: IMAKADO &lt;ken.imakado@gmail.com&gt;</td></tr><tr><th id="L5"><a href="#L5">5</a></th><td>;; Author's blog: http://d.hatena.ne.jp/IMAKADO (japanese)</td></tr><tr><th id="L6"><a href="#L6">6</a></th><td>;; Prefix: init-loader-</td></tr><tr><th id="L7"><a href="#L7">7</a></th><td></td></tr><tr><th id="L8"><a href="#L8">8</a></th><td>;; This file is free software; you can redistribute it and/or modify</td></tr><tr><th id="L9"><a href="#L9">9</a></th><td>;; it under the terms of the GNU General Public License as published by</td></tr><tr><th id="L10"><a href="#L10">10</a></th><td>;; the Free Software Foundation; either version 2, or (at your option)</td></tr><tr><th id="L11"><a href="#L11">11</a></th><td>;; any later version.</td></tr><tr><th id="L12"><a href="#L12">12</a></th><td></td></tr><tr><th id="L13"><a href="#L13">13</a></th><td>;; This file is distributed in the hope that it will be useful,</td></tr><tr><th id="L14"><a href="#L14">14</a></th><td>;; but WITHOUT ANY WARRANTY; without even the implied warranty of</td></tr><tr><th id="L15"><a href="#L15">15</a></th><td>;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</td></tr><tr><th id="L16"><a href="#L16">16</a></th><td>;; GNU General Public License for more details.</td></tr><tr><th id="L17"><a href="#L17">17</a></th><td></td></tr><tr><th id="L18"><a href="#L18">18</a></th><td>;; You should have received a copy of the GNU General Public License</td></tr><tr><th id="L19"><a href="#L19">19</a></th><td>;; along with GNU Emacs; see the file COPYING.  If not, write to the</td></tr><tr><th id="L20"><a href="#L20">20</a></th><td>;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,</td></tr><tr><th id="L21"><a href="#L21">21</a></th><td>;; Boston, MA 02110-1301, USA.</td></tr><tr><th id="L22"><a href="#L22">22</a></th><td></td></tr><tr><th id="L23"><a href="#L23">23</a></th><td>;;; 使い方</td></tr><tr><th id="L24"><a href="#L24">24</a></th><td>;; load-pathの通った場所に置いて</td></tr><tr><th id="L25"><a href="#L25">25</a></th><td>;; (require 'init-loader)</td></tr><tr><th id="L26"><a href="#L26">26</a></th><td>;; (init-loader-load "/path/to/init-directory")</td></tr><tr><th id="L27"><a href="#L27">27</a></th><td></td></tr><tr><th id="L28"><a href="#L28">28</a></th><td>;;  デフォルト設定の場合,以下の順序で引数に渡したディレクトリ以下のファイルをロードする.</td></tr><tr><th id="L29"><a href="#L29">29</a></th><td>;; 引数が省略された場合は,変数`init-loader-directory'の値を使用する.デフォルトは"~/.emacs.d/inits".</td></tr><tr><th id="L30"><a href="#L30">30</a></th><td></td></tr><tr><th id="L31"><a href="#L31">31</a></th><td>;; 1. ソートされた,二桁の数字から始まるファイル. e.x, "00_utils.el" "01_ik-cmd.el" "21_javascript.el" "99_global-keys.el"</td></tr><tr><th id="L32"><a href="#L32">32</a></th><td>;; 2. meadowの場合, meadow から始まる名前のファイル. e.x, "meadow-cmd.el" "meadow-config.el"</td></tr><tr><th id="L33"><a href="#L33">33</a></th><td>;; 3. carbon-emacsの場合, carbon-emacs から始まる名前のファイル. e.x, "carbon-emacs-config.el" "carbon-emacs-migemo.el"</td></tr><tr><th id="L34"><a href="#L34">34</a></th><td>;; 4. windowシステム以外の場合(terminal), nw から始まる名前のファイル e.x, "nw-config.el"</td></tr><tr><th id="L35"><a href="#L35">35</a></th><td></td></tr><tr><th id="L36"><a href="#L36">36</a></th><td>;; ファイルロード後,変数`init-loader-show-log-after-init'の値がnon-nilなら,ログバッファを表示する関数を`after-init-hook'へ追加する.</td></tr><tr><th id="L37"><a href="#L37">37</a></th><td>;; ログの表示は, M-x init-loader-show-log でも可能.</td></tr><tr><th id="L38"><a href="#L38">38</a></th><td></td></tr><tr><th id="L39"><a href="#L39">39</a></th><td>(eval-when-compile (require 'cl))</td></tr><tr><th id="L40"><a href="#L40">40</a></th><td>(require 'benchmark)</td></tr><tr><th id="L41"><a href="#L41">41</a></th><td></td></tr><tr><th id="L42"><a href="#L42">42</a></th><td>;;; customize-variables</td></tr><tr><th id="L43"><a href="#L43">43</a></th><td>(defgroup init-loader nil</td></tr><tr><th id="L44"><a href="#L44">44</a></th><td>  "init loader"</td></tr><tr><th id="L45"><a href="#L45">45</a></th><td>  :group 'init-loader)</td></tr><tr><th id="L46"><a href="#L46">46</a></th><td></td></tr><tr><th id="L47"><a href="#L47">47</a></th><td>(defcustom init-loader-directory (expand-file-name "~/.emacs.d/inits")</td></tr><tr><th id="L48"><a href="#L48">48</a></th><td>  "inits directory"</td></tr><tr><th id="L49"><a href="#L49">49</a></th><td>  :type 'directory</td></tr><tr><th id="L50"><a href="#L50">50</a></th><td>  :group 'init-loader)</td></tr><tr><th id="L51"><a href="#L51">51</a></th><td></td></tr><tr><th id="L52"><a href="#L52">52</a></th><td>(defcustom init-loader-show-log-after-init t</td></tr><tr><th id="L53"><a href="#L53">53</a></th><td>  "non-nilだと起動時にログバッファを表示する"</td></tr><tr><th id="L54"><a href="#L54">54</a></th><td>  :type 'boolean</td></tr><tr><th id="L55"><a href="#L55">55</a></th><td>  :group 'init-loader)</td></tr><tr><th id="L56"><a href="#L56">56</a></th><td></td></tr><tr><th id="L57"><a href="#L57">57</a></th><td>(defcustom init-loader-default-regexp "\\(?:^[[:digit:]]\\{2\\}\\)"</td></tr><tr><th id="L58"><a href="#L58">58</a></th><td>  "起動時に読み込まれる設定ファイルにマッチする正規表現.</td></tr><tr><th id="L59"><a href="#L59">59</a></th><td>デフォルトは二桁の数字から始まるファイルにマッチする正規表現.</td></tr><tr><th id="L60"><a href="#L60">60</a></th><td>e.x, 00_hoge.el, 01_huga.el ... 99_keybind.el"</td></tr><tr><th id="L61"><a href="#L61">61</a></th><td>  :type 'regexp</td></tr><tr><th id="L62"><a href="#L62">62</a></th><td>  :group 'init-loader )</td></tr><tr><th id="L63"><a href="#L63">63</a></th><td></td></tr><tr><th id="L64"><a href="#L64">64</a></th><td>(defcustom init-loader-meadow-regexp "^meadow-"</td></tr><tr><th id="L65"><a href="#L65">65</a></th><td>  "meadow 使用時に読み込まれる設定ファイルにマッチする正規表現"</td></tr><tr><th id="L66"><a href="#L66">66</a></th><td>  :group 'init-loader</td></tr><tr><th id="L67"><a href="#L67">67</a></th><td>  :type 'regexp)</td></tr><tr><th id="L68"><a href="#L68">68</a></th><td></td></tr><tr><th id="L69"><a href="#L69">69</a></th><td>(defcustom init-loader-carbon-emacs-regexp "^carbon-emacs-"</td></tr><tr><th id="L70"><a href="#L70">70</a></th><td>  "carbon-emacs 使用時に読み込まれる設定ファイルにマッチする正規表現"</td></tr><tr><th id="L71"><a href="#L71">71</a></th><td>  :group 'init-loader</td></tr><tr><th id="L72"><a href="#L72">72</a></th><td>  :type 'regexp)</td></tr><tr><th id="L73"><a href="#L73">73</a></th><td></td></tr><tr><th id="L74"><a href="#L74">74</a></th><td>(defcustom init-loader-cocoa-emacs-regexp "^cocoa-emacs-"</td></tr><tr><th id="L75"><a href="#L75">75</a></th><td>  "cocoa-emacs 使用時に読み込まれる設定ファイルにマッチする正規表現"</td></tr><tr><th id="L76"><a href="#L76">76</a></th><td>  :group 'init-loader</td></tr><tr><th id="L77"><a href="#L77">77</a></th><td>  :type 'regexp)</td></tr><tr><th id="L78"><a href="#L78">78</a></th><td></td></tr><tr><th id="L79"><a href="#L79">79</a></th><td>(defcustom init-loader-nw-regexp "^nw-"</td></tr><tr><th id="L80"><a href="#L80">80</a></th><td>  "no-window環境での起動時に読み込まれる設定ファイルにマッチする正規表現"</td></tr><tr><th id="L81"><a href="#L81">81</a></th><td>  :group 'init-loader</td></tr><tr><th id="L82"><a href="#L82">82</a></th><td>  :type 'regexp)</td></tr><tr><th id="L83"><a href="#L83">83</a></th><td></td></tr><tr><th id="L84"><a href="#L84">84</a></th><td>;;; Code</td></tr><tr><th id="L85"><a href="#L85">85</a></th><td>(defun* init-loader-load (&amp;optional (init-dir init-loader-directory))</td></tr><tr><th id="L86"><a href="#L86">86</a></th><td>  (let ((init-dir (init-loader-follow-symlink init-dir)))</td></tr><tr><th id="L87"><a href="#L87">87</a></th><td>    (assert (and (stringp init-dir) (file-directory-p init-dir)))</td></tr><tr><th id="L88"><a href="#L88">88</a></th><td>    (init-loader-re-load init-loader-default-regexp init-dir t)</td></tr><tr><th id="L89"><a href="#L89">89</a></th><td>    ;; meadow</td></tr><tr><th id="L90"><a href="#L90">90</a></th><td>    (and (featurep 'meadow)</td></tr><tr><th id="L91"><a href="#L91">91</a></th><td>         (init-loader-re-load init-loader-meadow-regexp init-dir))</td></tr><tr><th id="L92"><a href="#L92">92</a></th><td>    ;; carbon emacs</td></tr><tr><th id="L93"><a href="#L93">93</a></th><td>    (and (featurep 'carbon-emacs-package)</td></tr><tr><th id="L94"><a href="#L94">94</a></th><td>         (init-loader-re-load init-loader-carbon-emacs-regexp init-dir))</td></tr><tr><th id="L95"><a href="#L95">95</a></th><td>    ;; cocoa emacs</td></tr><tr><th id="L96"><a href="#L96">96</a></th><td>    (and (equal window-system 'ns)</td></tr><tr><th id="L97"><a href="#L97">97</a></th><td>         (init-loader-re-load init-loader-cocoa-emacs-regexp init-dir))</td></tr><tr><th id="L98"><a href="#L98">98</a></th><td>    ;; no window</td></tr><tr><th id="L99"><a href="#L99">99</a></th><td>    (and (null window-system)</td></tr><tr><th id="L100"><a href="#L100">100</a></th><td>         (init-loader-re-load init-loader-nw-regexp init-dir))</td></tr><tr><th id="L101"><a href="#L101">101</a></th><td></td></tr><tr><th id="L102"><a href="#L102">102</a></th><td>    (when init-loader-show-log-after-init</td></tr><tr><th id="L103"><a href="#L103">103</a></th><td>      (add-hook  'after-init-hook 'init-loader-show-log))))</td></tr><tr><th id="L104"><a href="#L104">104</a></th><td></td></tr><tr><th id="L105"><a href="#L105">105</a></th><td>(defun init-loader-follow-symlink (dir)</td></tr><tr><th id="L106"><a href="#L106">106</a></th><td>  (cond ((file-symlink-p dir)</td></tr><tr><th id="L107"><a href="#L107">107</a></th><td>         (expand-file-name (file-symlink-p dir)))</td></tr><tr><th id="L108"><a href="#L108">108</a></th><td>        (t (expand-file-name dir))))</td></tr><tr><th id="L109"><a href="#L109">109</a></th><td></td></tr><tr><th id="L110"><a href="#L110">110</a></th><td>(lexical-let (logs)</td></tr><tr><th id="L111"><a href="#L111">111</a></th><td>  (defun init-loader-log (&amp;optional s)</td></tr><tr><th id="L112"><a href="#L112">112</a></th><td>    (if s (and (stringp s) (push s logs)) (mapconcat 'identity (reverse logs) "\n"))))</td></tr><tr><th id="L113"><a href="#L113">113</a></th><td></td></tr><tr><th id="L114"><a href="#L114">114</a></th><td>(lexical-let (err-logs)</td></tr><tr><th id="L115"><a href="#L115">115</a></th><td>  (defun init-loader-error-log (&amp;optional s)</td></tr><tr><th id="L116"><a href="#L116">116</a></th><td>    (if s (and (stringp s) (push s err-logs)) (mapconcat 'identity (reverse err-logs) "\n"))))</td></tr><tr><th id="L117"><a href="#L117">117</a></th><td></td></tr><tr><th id="L118"><a href="#L118">118</a></th><td>(defun init-loader-re-load (re dir &amp;optional sort)</td></tr><tr><th id="L119"><a href="#L119">119</a></th><td>  (let ((load-path (cons dir load-path)))</td></tr><tr><th id="L120"><a href="#L120">120</a></th><td>    (dolist (el (init-loader--re-load-files re dir sort))</td></tr><tr><th id="L121"><a href="#L121">121</a></th><td>      (condition-case e</td></tr><tr><th id="L122"><a href="#L122">122</a></th><td>          (let ((time (car (benchmark-run (load (file-name-sans-extension el))))))</td></tr><tr><th id="L123"><a href="#L123">123</a></th><td>            (init-loader-log (format "loaded %s. %s" (locate-library el) time)))</td></tr><tr><th id="L124"><a href="#L124">124</a></th><td>        (error</td></tr><tr><th id="L125"><a href="#L125">125</a></th><td>         (init-loader-error-log (error-message-string e)))))))</td></tr><tr><th id="L126"><a href="#L126">126</a></th><td></td></tr><tr><th id="L127"><a href="#L127">127</a></th><td>(defun init-loader--re-load-files (re dir &amp;optional sort)</td></tr><tr><th id="L128"><a href="#L128">128</a></th><td>    (loop for el in (directory-files dir t)</td></tr><tr><th id="L129"><a href="#L129">129</a></th><td>          when (string-match re (file-name-nondirectory el))</td></tr><tr><th id="L130"><a href="#L130">130</a></th><td>          collect (file-name-nondirectory el) into ret</td></tr><tr><th id="L131"><a href="#L131">131</a></th><td>          finally return (if sort (sort ret 'string&lt;) ret)))</td></tr><tr><th id="L132"><a href="#L132">132</a></th><td></td></tr><tr><th id="L133"><a href="#L133">133</a></th><td>(defun init-loader-show-log ()</td></tr><tr><th id="L134"><a href="#L134">134</a></th><td>  "return buffer"</td></tr><tr><th id="L135"><a href="#L135">135</a></th><td>  (interactive)</td></tr><tr><th id="L136"><a href="#L136">136</a></th><td>  (let ((b (get-buffer-create "*init log*")))</td></tr><tr><th id="L137"><a href="#L137">137</a></th><td>    (with-current-buffer b</td></tr><tr><th id="L138"><a href="#L138">138</a></th><td>      (erase-buffer)</td></tr><tr><th id="L139"><a href="#L139">139</a></th><td>      (insert "------- error log -------\n\n"</td></tr><tr><th id="L140"><a href="#L140">140</a></th><td>              (init-loader-error-log)</td></tr><tr><th id="L141"><a href="#L141">141</a></th><td>              "\n\n")</td></tr><tr><th id="L142"><a href="#L142">142</a></th><td>      (insert "------- init log -------\n\n"</td></tr><tr><th id="L143"><a href="#L143">143</a></th><td>              (init-loader-log)</td></tr><tr><th id="L144"><a href="#L144">144</a></th><td>              "\n\n")</td></tr><tr><th id="L145"><a href="#L145">145</a></th><td>      ;; load-path</td></tr><tr><th id="L146"><a href="#L146">146</a></th><td>      (insert "------- load path -------\n\n"</td></tr><tr><th id="L147"><a href="#L147">147</a></th><td>              (mapconcat 'identity load-path "\n"))</td></tr><tr><th id="L148"><a href="#L148">148</a></th><td>      (goto-char (point-min)))</td></tr><tr><th id="L149"><a href="#L149">149</a></th><td>    (switch-to-buffer b)))</td></tr><tr><th id="L150"><a href="#L150">150</a></th><td></td></tr><tr><th id="L151"><a href="#L151">151</a></th><td></td></tr><tr><th id="L152"><a href="#L152">152</a></th><td>;;;; Test</td></tr><tr><th id="L153"><a href="#L153">153</a></th><td>(defvar init-loader-test-files</td></tr><tr><th id="L154"><a href="#L154">154</a></th><td>  '("00_utils.el"</td></tr><tr><th id="L155"><a href="#L155">155</a></th><td>    "23_yaml.el"</td></tr><tr><th id="L156"><a href="#L156">156</a></th><td>    "01_ik-cmd.el"</td></tr><tr><th id="L157"><a href="#L157">157</a></th><td>    "96_color.el"</td></tr><tr><th id="L158"><a href="#L158">158</a></th><td>    "20_elisp.el"</td></tr><tr><th id="L159"><a href="#L159">159</a></th><td>    "21_javascript.el"</td></tr><tr><th id="L160"><a href="#L160">160</a></th><td>    "25_perl.el"</td></tr><tr><th id="L161"><a href="#L161">161</a></th><td>    "98_emacs-config.el"</td></tr><tr><th id="L162"><a href="#L162">162</a></th><td>    "99_global-keys.el"</td></tr><tr><th id="L163"><a href="#L163">163</a></th><td>    "carbon-emacs-config.el"</td></tr><tr><th id="L164"><a href="#L164">164</a></th><td>    "carbon-emacs-migemo.el"</td></tr><tr><th id="L165"><a href="#L165">165</a></th><td>    "nw-config.el"</td></tr><tr><th id="L166"><a href="#L166">166</a></th><td>    "emacs-migemo.el"</td></tr><tr><th id="L167"><a href="#L167">167</a></th><td>    "meadow-cmd.el"</td></tr><tr><th id="L168"><a href="#L168">168</a></th><td>    "meadow-config.el"</td></tr><tr><th id="L169"><a href="#L169">169</a></th><td>    "meadow-gnuserv.el"</td></tr><tr><th id="L170"><a href="#L170">170</a></th><td>    "meadow-shell.el"</td></tr><tr><th id="L171"><a href="#L171">171</a></th><td>    "meadow-w32-symlinks.el"))</td></tr><tr><th id="L172"><a href="#L172">172</a></th><td></td></tr><tr><th id="L173"><a href="#L173">173</a></th><td>(dont-compile</td></tr><tr><th id="L174"><a href="#L174">174</a></th><td>  (when (fboundp 'expectations)</td></tr><tr><th id="L175"><a href="#L175">175</a></th><td>    (expectations </td></tr><tr><th id="L176"><a href="#L176">176</a></th><td>      (desc "init-loader--re-load-files")</td></tr><tr><th id="L177"><a href="#L177">177</a></th><td>      (expect  '("00_utils.el" "01_ik-cmd.el" "20_elisp.el" "21_javascript.el" "23_yaml.el" "25_perl.el" "96_color.el" "98_emacs-config.el" "99_global-keys.el")</td></tr><tr><th id="L178"><a href="#L178">178</a></th><td>        (stub directory-files =&gt; init-loader-test-files)</td></tr><tr><th id="L179"><a href="#L179">179</a></th><td>        (init-loader--re-load-files init-loader-default-regexp "" t))</td></tr><tr><th id="L180"><a href="#L180">180</a></th><td>      (expect  '("meadow-cmd.el" "meadow-config.el" "meadow-gnuserv.el" "meadow-shell.el" "meadow-w32-symlinks.el")</td></tr><tr><th id="L181"><a href="#L181">181</a></th><td>        (stub directory-files =&gt; init-loader-test-files)</td></tr><tr><th id="L182"><a href="#L182">182</a></th><td>        (init-loader--re-load-files init-loader-meadow-regexp "" t))</td></tr><tr><th id="L183"><a href="#L183">183</a></th><td></td></tr><tr><th id="L184"><a href="#L184">184</a></th><td>      (expect  '("carbon-emacs-config.el" "carbon-emacs-migemo.el")</td></tr><tr><th id="L185"><a href="#L185">185</a></th><td>        (stub directory-files =&gt; init-loader-test-files)</td></tr><tr><th id="L186"><a href="#L186">186</a></th><td>        (init-loader--re-load-files init-loader-carbon-emacs-regexp "" t))</td></tr><tr><th id="L187"><a href="#L187">187</a></th><td>      (expect  '("nw-config.el")</td></tr><tr><th id="L188"><a href="#L188">188</a></th><td>        (stub directory-files =&gt; init-loader-test-files)</td></tr><tr><th id="L189"><a href="#L189">189</a></th><td>        (init-loader--re-load-files init-loader-nw-regexp "" t))</td></tr><tr><th id="L190"><a href="#L190">190</a></th><td>      ;; 環境依存</td></tr><tr><th id="L191"><a href="#L191">191</a></th><td>      (desc "follow symlink")</td></tr><tr><th id="L192"><a href="#L192">192</a></th><td>      (expect "c/.emacs.d/inits"</td></tr><tr><th id="L193"><a href="#L193">193</a></th><td>        (file-relative-name (file-symlink-p "~/tmp/el-inits"))) ; symlink</td></tr><tr><th id="L194"><a href="#L194">194</a></th><td>      (desc "init-loader-follow-symlink")</td></tr><tr><th id="L195"><a href="#L195">195</a></th><td>      (expect "c/.emacs.d/inits"</td></tr><tr><th id="L196"><a href="#L196">196</a></th><td>        (file-relative-name (init-loader-follow-symlink "~/tmp/el-inits")))</td></tr><tr><th id="L197"><a href="#L197">197</a></th><td>      (expect "c/.emacs.d/inits"</td></tr><tr><th id="L198"><a href="#L198">198</a></th><td>        (file-relative-name (init-loader-follow-symlink "~/tmp/el-inits")))</td></tr><tr><th id="L199"><a href="#L199">199</a></th><td>      )))</td></tr><tr><th id="L200"><a href="#L200">200</a></th><td></td></tr><tr><th id="L201"><a href="#L201">201</a></th><td>(provide 'init-loader)</td></tr><tr><th id="L202"><a href="#L202">202</a></th><td></td></tr></tbody></table>
      </div>
      <div id="help">
        <strong>Note:</strong> See <a href="/share/wiki/TracBrowser">TracBrowser</a>
        for help on using the browser.
      </div>
      <div id="anydiff">
        <form action="/share/diff" method="get">
          <div class="buttons">
            <input type="hidden" name="new_path" value="/lang/elisp/init-loader/init-loader.el" />
            <input type="hidden" name="old_path" value="/lang/elisp/init-loader/init-loader.el" />
            <input type="hidden" name="new_rev" value="28647" />
            <input type="hidden" name="old_rev" value="28647" />
            <input type="submit" value="View changes..." title="Select paths and revs for Diff" />
          </div>
        </form>
      </div>
    </div>
    <div id="altlinks">
      <h3>Download in other formats:</h3>
      <ul>
        <li class="first">
          <a rel="nofollow" href="/share/browser/lang/elisp/init-loader/init-loader.el?format=txt">Plain Text</a>
        </li><li class="last">
          <a rel="nofollow" href="/share/export/39278/lang/elisp/init-loader/init-loader.el">Original Format</a>
        </li>
      </ul>
    </div>
    </div>
    <div id="footer" lang="en" xml:lang="en"><hr />
      <a id="tracpowered" href="http://trac.edgewall.org/"><img src="/share/chrome/common/trac_logo_mini.png" height="30" width="107" alt="Trac Powered" /></a>
      <p class="left">
        Powered by <a href="/share/about"><strong>Trac 0.11</strong></a><br />
        By <a href="http://www.edgewall.org/">Edgewall Software</a>.
      </p>
      <p class="right">Visit the Trac open source project at<br /><a href="http://trac.edgewall.org/">http://trac.edgewall.org/</a></p>
    </div>
  </body>
</html>