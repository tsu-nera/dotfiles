
// Tips are here http://conkeror.org/Tips
homepage = "http://www.google.co.jp";

///////////////////////////////////////////////////////////////
//  ÉÇÅ[ÉhÉâÉCÉì
//  http://conkeror.org/ModeLine?highlight=%28mode-line%29
///////////////////////////////////////////////////////////////
// funky icons in the modeline
require("mode-line.js");

load_paths.unshift("chrome://conkeror-contrib/content/");
require("mode-line-buttons.js");
mode_line_add_buttons(standard_mode_line_buttons, true);

/////////////////////////////////////////////////////////
// History 
// http://conkeror.org/History?highlight=%28history%29
////////////////////////////////////////////////////////
// http://conkeror.org/Tips#Browse_buffer_session_history
interactive("browse-buffer-history",
    "Browse the session history for the current buffer",
    function browse_buffer_history (I) {
        var b = check_buffer(I.buffer, content_buffer);
        var history = b.web_navigation.sessionHistory;

        if (history.count > 1) {
            var entries = [];

            for(var i = 0 ; i < history.count ; i += 1) {
                entries[i] = history.getEntryAtIndex(i, false).URI.spec;
            }

            var url = yield I.minibuffer.read(
                $prompt = "Go back or forward to:",
                $completer = new all_word_completer($completions = entries),
                $default_completion = history.index > 0 ? entries[history.index - 1] : entries[history.index + 1],
                $auto_complete = "url",
                $auto_complete_initial = true,
                $auto_complete_delay = 0,
                $require_match = true);

            b.web_navigation.gotoIndex(entries.indexOf(url));
        } else {
            I.window.minibuffer.message("No history");
        }
    });

define_webjump("buffer-history", "browse_buffer_history");

url_completion_use_history = true; // should work since bf05c87405

// history webjump
define_browser_object_class(
    "history-url", null, 
    function (I, prompt) {
        check_buffer (I.buffer, content_buffer);
        var result = yield I.buffer.window.minibuffer.read_url(
            $prompt = prompt,  $use_webjumps = false, $use_history = true, $use_bookmarks = false);
        yield co_return (result);
    });

interactive("find-url-from-history",
            "Find a page from history in the current buffer",
            "find-url",
            $browser_object = browser_object_history_url);

interactive("find-url-from-history-new-buffer",
            "Find a page from history in the current buffer",
            "find-url-new-buffer",
            $browser_object = browser_object_history_url);

define_key(content_buffer_normal_keymap, "h", "find-url-from-history-new-buffer");
define_key(content_buffer_normal_keymap, "H", "find-url-from-history");

//////////////////////////////////////////
// Key Bindings
//////////////////////////////////////////
define_key (minibuffer_keymap, "C-m", "exit-minibuffer");

//////////////////////////////////////////
// Big Hint Number
/////////////////////////////////////////
register_user_stylesheet(
    "data:text/css," +
        escape(
            "@namespace url(\"http://www.w3.org/1999/xhtml\");\n" +
            "span.__conkeror_hint {\n"+
            "  font-size: 18px !important;\n"+
            "  line-height: 18px !important;\n"+
            "}"));

// give me new tabs; open buffers (tabs) in the background
//require("tab-bar.js");
require("new-tabs.js");
require("clicks-in-new-buffer.js");
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND;
clicks_in_new_buffer_button = 1; //  midclick links in new buffers with

// buffer number ÇéwíËÇµÇƒêÿÇËë÷Ç¶ÇÈ
function define_switch_buffer_key (key, buf_num) {
    define_key(default_global_keymap, key,
               function (I) {
                   switch_to_buffer(I.window,
                                    I.window.buffers.get_buffer(buf_num));
               });
}
for (let i = 0; i < 10; ++i) {
    define_switch_buffer_key(String((i+1)%10), i);
}

// auto completion in the minibuffer
minibuffer_auto_complete_default = true;
url_completion_use_bookmarks = true;

//////////////////////////////////////////
// favicon
//////////////////////////////////////////
// http://conkeror.org/Favicons
require("favicon");
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
read_buffer_show_icons = true;

//////////////////////////////////////////
// shortcut
//////////////////////////////////////////
interactive("open-google", "Go to google.co.jp", "follow",
    $browser_object = "http://www.google.co.jp/");
interactive("open-gmail", "Go to gmail", "follow",
    $browser_object = "http://gmail.com/");
interactive("open-calendar", "Go to calendar.google.com", "follow",
    $browser_object = "http://calendar.google.com/");
interactive("futurismo", "Open Futurismo", "follow",
    $browser_object = "http://futurismo.biz");
interactive("futurismo_wiki", "Open Futurismo", "follow",
    $browser_object = "http://futurismo.biz/dokuwiki");
interactive("youtube", "Open Youtube", "follow",
    $browser_object = "http://www.youtube.com");
interactive("feedly", "Open Feedly", "follow",
    $browser_object = "http://feedly.com");
interactive("tomatoes", "Open Tomatoes", "follow",
    $browser_object = "http://tomato.es/");
interactive("github", "Open Github", "follow",
            $browser_object = "https://github.com/tsu-nera");
interactive("studyplus", "Open Studyplus", "follow",
            $browser_object = "https://studyplus.jp/home");
interactive("twitter", "Open Twitter", "follow",
            $browser_object = "https://twitter.com/?lang=ja");
interactive("youtube-dl", "download youtube video",
            function (I) {
                shell_command_blind("youtube-dl " + I.buffer.display_uri_string);
            });

// open url with new buffer
define_key(content_buffer_normal_keymap, "d", "follow-new-buffer");
define_key(content_buffer_normal_keymap, "f1", "open-google");
define_key(content_buffer_normal_keymap, "f2", "open-gmail");
define_key(content_buffer_normal_keymap, "f3", "twitter");
define_key(content_buffer_normal_keymap, "f4", "youtube");
define_key(content_buffer_normal_keymap, "f5", "futurismo");
define_key(content_buffer_normal_keymap, "f6", "futurismo_wiki");
define_key(content_buffer_normal_keymap, "f7", "github");
define_key(content_buffer_normal_keymap, "f8", "studyplus");

//////////////////////////////////////////
// webjump
//////////////////////////////////////////
// evernote
define_webjump("clip","javascript:(function(){EN_CLIP_HOST='http://www.evernote.com';try{var%20x=document.createElement('SCRIPT');x.type='text/javascript';x.src=EN_CLIP_HOST+'/public/bookmarkClipper.js?'+(new%20Date().getTime()/100000);document.getElementsByTagName('head')[0].appendChild(x);}catch(e){location.href=EN_CLIP_HOST+'/clip.action?url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title);}})();");

// hatenabookmark
// http://b.hatena.ne.jp/register
define_webjump("hatena","javascript:(function(){var%20d=(new%20Date);var%20s=document.createElement('script');s.charset='UTF-8';s.src='http://b.hatena.ne.jp/js/Hatena/Bookmark/let.js?'+d.getFullYear()+d.getMonth()+d.getDate();(document.getElementsByTagName('head')[0]||document.body).appendChild(s);})();");

// twitter
define_webjump("twitter","javascript:(function()%7Bwindow.twttr=window.twttr%7C%7C%7B%7D;var%20D=550,A=450,C=screen.height,B=screen.width,H=Math.round((B/2)-(D/2)),G=0,F=document,E;if(C%3EA)%7BG=Math.round((C/2)-(A/2))%7Dwindow.twttr.shareWin=window.open(%27http://twitter.com/share%27,%27%27,%27left=%27+H+%27,top=%27+G+%27,width=%27+D+%27,height=%27+A+%27,personalbar=0,toolbar=0,scrollbars=1,resizable=1%27);E=F.createElement(%27script%27);E.src=%27http://platform.twitter.com/bookmarklets/share.js?v=1%27;F.getElementsByTagName(%27head%27)%5B0%5D.appendChild(E)%7D());");

// readability
define_webjump("readability","javascript:(%0A%28function%28%29%7Bwindow.baseUrl%3D%27//www.readability.com%27%3Bwindow.readabilityToken%3D%27DsMgYF6muHA9dzMgEGyLPuMVRYGxf5DFBRVtx9kf%27%3Bvar%20s%3Ddocument.createElement%28%27script%27%29%3Bs.setAttribute%28%27type%27%2C%27text/javascript%27%29%3Bs.setAttribute%28%27charset%27%2C%27UTF-8%27%29%3Bs.setAttribute%28%27src%27%2CbaseUrl%2B%27/bookmarklet/save.js%27%29%3Bdocument.documentElement.appendChild%28s%29%3B%7D%29%28%29);");

// create link
// define_webjump("link_markdown","javascript:(function(){prompt('Copy to Clipboard','['+document.title.replace(/([\[\]])/g,'\\$1')+']'+'('+location.href+')');})();");
define_webjump("@","javascript:(function(){prompt('Copy to Clipboard','['+'['+location.href+']'+'['+document.title.replace(/([\[\]])/g,'\\$1')+']'+']');})();");

// google search with japanese
define_webjump("g", "http://www.google.co.jp/search?q=%s", $alternative = "http://www.google.co.jp/");

//////////////////////////////////////////
// external
//////////////////////////////////////////
// automatically handle some mime types internally.
content_handlers.set("application/pdf", content_handler_open);

// external programs for handling various mime types.
external_content_handlers.set("application/pdf", "FoxitReader");
external_content_handlers.set("video/*", "xterm -e vlc");

///////////////////////////////////////////////////////////////
//  Others
///////////////////////////////////////////////////////////////
// view source in your editor.
view_source_use_external_editor = true;

// keep session
require("session.js");

// Use emacsclient as external editor
editor_shell_command = "emacsclient -c"

// default directory for downloads and shell commands.
cwd = get_home_directory();
cwd.append("Downloads");

// load download buffers in the background in the current
// window, instead of in new windows.
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

interactive("reload-config", "reload conkerorrc",
            function(I) {
                load_rc();
                I.window.minibuffer.message("config reloaded");
            });
define_key(default_global_keymap, "C-c r", "reload-config");

////////////////////////////////////////////
// colors DÇ≈çïîwåiÇ… toggle
////////////////////////////////////////////
theme_load_paths.unshift("~/.conkerorrc/themes/");
theme_unload("default");
theme_load("conkeror-theme-zenburn");

////////////////////////////////////////////
/// Password Manager
/// http://conkeror.org/PasswordManagement
///////////////////////////////////////////
session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
session_pref("signon.SignonFileName", "signons.txt");
Cc["@mozilla.org/login-manager;1"].getService(Ci.nsILoginManager);
