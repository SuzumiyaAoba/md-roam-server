# md-roam-server Docker セットアップ

このディレクトリには、md-roam-server を Docker で実行するための設定ファイルが含まれています。

## 構成

- `docker-compose.yml` - メインの Docker Compose 設定
- `Dockerfile` - マルチステージビルド用の Dockerfile
- `Dockerfile.simple` - シンプルなシングルステージビルド用の Dockerfile
- `config.yml` - コンテナ用の設定ファイル
- `start-docker.sh` - コンテナ内での起動スクリプト
- `start.sh` - カスタムポートでの起動スクリプト
- `env.example` - 環境変数の設定例

## クイックスタート

### 基本的な起動

```bash
# デフォルト設定で起動
docker compose up -d

# ログを確認
docker compose logs -f

# 停止
docker compose down
```

### カスタムポートでの起動

```bash
# REST APIを9000ポート、org-roamを36000ポートで起動
./docker/start.sh -r 9000

# カスタム設定ファイルで起動
./docker/start.sh --config /path/to/config.yml

# 本番環境用
./docker/start.sh -r 80 --production

# 監視付き
./docker/start.sh --monitoring -p 9091
```

## 環境変数

| 変数名 | デフォルト値 | 説明 |
|--------|--------------|------|
| `REST_API_PORT` | 8080 | REST APIのポート番号 |
| `MD_ROAM_CONFIG_FILE` | | カスタム設定ファイルのパス |

## 設定ファイル

`config.yml` で以下の設定が可能です：

```yaml
server:
  port: 8080                    # REST APIポート

org-roam:
  directory: /data/org-roam     # org-roamデータディレクトリ
```

## データ永続化

コンテナのデータは以下のボリュームで永続化されます：

- `org-roam-data` - org-roam のノートファイル
- `config-data` - 設定ファイル

## トラブルシューティング

### ポートが既に使用されている場合

```bash
# 使用中のポートを確認
lsof -i :8080

# プロセスを停止
pkill -f "md-roam-server"
```

### ログの確認

```bash
# コンテナのログを確認
docker compose logs -f md-roam-server

# 特定の時間以降のログ
docker compose logs --since="2024-01-01T00:00:00" md-roam-server
```

### コンテナ内での操作

```bash
# コンテナ内でシェルを開く
docker compose exec md-roam-server bash

# 設定ファイルを確認
docker compose exec md-roam-server cat ~/.config/md-roam-server/config.yml
```
