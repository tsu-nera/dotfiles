# dotfiles

設定はエージェントが読み書きする前提。このファイルが運用ルールの正本（`AGENTS.md` は symlink）。

## 対象マシン
- `mouse`: CachyOS（主機）
- `vaio`: EndeavourOS（`ssh vaio`。login shell は bash で対話時のみ `exec fish`、`.bashrc` は dotfiles 管理外）
- Windows の MSYS2（`env.fish` の `$MSYSTEM` 分岐）

## 適用
- `./make_lnlink`: リポジトリのファイルを `$HOME` の同じ相対パスへファイル単位で symlink（冪等・上書きしない）
- `./make_lnlink --check`: 変更せずに未リンク・競合・壊れたリンクを報告。変更後は全マシンでこれが exit 0 になること
- 他マシンへの反映は push してから `ssh vaio 'cd ~/repo/dotfiles && git pull --ff-only && ./make_lnlink --check'`
- ファイルを消したら各マシンの `dangling:` の symlink も消す

## 共通 / マシン固有の分け方
- symlink されたファイル = 全マシン共通。マシン固有は symlink しない実ファイルに置く:
  - fish: `~/.config/fish/local.fish`（config.fish が最後に読む）
  - git: `~/.gitconfig.local`（`[include]` で最後に読む）
- 存在チェックで済むもの（PATH 等。`fish_add_path` は存在しない dir を無視する）は共通側に書く。「このマシンではこうする」という判断だけ固有側に書く。ホスト名や OS の if 分岐を共通ファイルに増やさない
- fish の PATH は `env.fish` の `fish_add_path -g` だけ。universal 変数（`set -U`、`fish_user_paths`）に設定を置かない
- プロンプト（pure）と autopair は fish の vendor_conf.d（パッケージ `fish-pure-prompt` `fish-autopair`）から来る。リポジトリには無い

## 削除してよい基準（確認できれば事後報告でよい）
- 対象のツール・パス・環境（Cygwin・xmonad・zsh 等）がどのマシンにも無い
- 到達しないコード、重複定義、プラグインマネージャの残骸
- alias / 関数で、全マシンの `~/.local/share/fish/fish_history` で使用 0 回かつ追加から1年以上
- 上に当てはまらない「使っていなさそう」は消さずに一覧で聞く
- リポジトリ外（`~` や `~/.config`）は git で戻せないので、削除前に `~/legacy-archive-YYYYMMDD.tar.gz` へ tar してから消す。キャッシュや大きなデータ（conda 環境・ツールチェーン等）は基準を満たしても聞く

## 書き方
- コメントは「なぜ」だけ。何をしているかはコードから読めるので書かない
- 他所で既定値になっている設定は書かない

## 変更後の確認
- fish: `fish -i -c exit` がエラーなし（`time` で起動 0.1s 未満が目安）
- git: `git config --list --show-origin >/dev/null`
- tmux: `tmux -L t -f .tmux.conf new-session -d \; kill-server` がエラーなし
