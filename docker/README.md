# Docker Compose での起動方法

## 基本的な起動方法

```bash
# デフォルトポートで起動
docker-compose up -d

# デフォルトポート:
# - REST API: 8080
# - org-roam-ui: 35901
```

## カスタムポートでの起動

環境変数を使用してポート番号を指定できます：

```bash
# REST APIを9000ポート、org-roam-uiを36000ポートで起動
REST_API_PORT=9000 ORG_ROAM_UI_PORT=36000 docker-compose up -d

# または、.envファイルを作成して設定
echo "REST_API_PORT=9000" > .env
echo "ORG_ROAM_UI_PORT=36000" >> .env
docker-compose up -d
```

## 利用可能な環境変数

| 環境変数 | デフォルト値 | 説明 |
|---------|-------------|------|
| `REST_API_PORT` | 8080 | REST APIのポート番号 |
| `ORG_ROAM_UI_PORT` | 35901 | org-roam-uiのポート番号 |
| `TRAEFIK_PORT` | 80 | Traefikのポート番号（productionプロファイル使用時） |
| `TRAEFIK_DASHBOARD_PORT` | 8081 | Traefikダッシュボードのポート番号（productionプロファイル使用時） |
| `PROMETHEUS_PORT` | 9090 | Prometheusのポート番号（monitoringプロファイル使用時） |

## プロファイル付きでの起動

```bash
# 本番環境用（Traefik付き）
docker-compose --profile production up -d

# 監視付きで起動
docker-compose --profile monitoring up -d

# 両方のプロファイルを有効化
docker-compose --profile production --profile monitoring up -d
```

## 使用例

```bash
# 開発環境用（カスタムポート）
REST_API_PORT=3000 ORG_ROAM_UI_PORT=3001 docker-compose up -d

# 本番環境用（カスタムポート）
REST_API_PORT=80 ORG_ROAM_UI_PORT=443 TRAEFIK_PORT=8080 TRAEFIK_DASHBOARD_PORT=8081 docker-compose --profile production up -d
```

## 便利な起動スクリプト

`start.sh`スクリプトを使用すると、より簡単にカスタムポートで起動できます：

```bash
# 基本的なカスタムポート起動
./docker/start.sh -r 9000 -u 36000

# 本番環境用
./docker/start.sh -r 80 -u 443 --production

# 監視付き
./docker/start.sh --monitoring -p 9091

# ヘルプを表示
./docker/start.sh --help
```
