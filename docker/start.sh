#!/bin/bash

# md-roam-server カスタムポート起動スクリプト

set -e

# デフォルト値
DEFAULT_REST_API_PORT=8080
DEFAULT_TRAEFIK_PORT=80
DEFAULT_TRAEFIK_DASHBOARD_PORT=8081
DEFAULT_PROMETHEUS_PORT=9090

# ヘルプメッセージ
show_help() {
    cat << EOF
md-roam-server カスタムポート起動スクリプト

使用方法:
    $0 [オプション]

オプション:
    -r, --rest-port PORT      REST APIポート (デフォルト: $DEFAULT_REST_API_PORT)
    -t, --traefik-port PORT   Traefikポート (デフォルト: $DEFAULT_TRAEFIK_PORT)
    -d, --dashboard-port PORT Traefikダッシュボードポート (デフォルト: $DEFAULT_TRAEFIK_DASHBOARD_PORT)
    -p, --prometheus-port PORT Prometheusポート (デフォルト: $DEFAULT_PROMETHEUS_PORT)
    --config PATH             設定ファイルのパスを指定
    --production              本番環境プロファイルを有効化
    --monitoring              監視プロファイルを有効化
    --env-file FILE           .envファイルのパスを指定
    -h, --help                このヘルプを表示

例:
    # 基本的なカスタムポート起動
    $0 -r 9000

    # カスタム設定ファイルで起動
    $0 --config /path/to/config.yml

    # 本番環境用
    $0 -r 80 --production

    # 監視付き
    $0 --monitoring -p 9091

    # .envファイル使用
    $0 --env-file .env.production
EOF
}

# 変数の初期化
REST_API_PORT=$DEFAULT_REST_API_PORT
TRAEFIK_PORT=$DEFAULT_TRAEFIK_PORT
TRAEFIK_DASHBOARD_PORT=$DEFAULT_TRAEFIK_DASHBOARD_PORT
PROMETHEUS_PORT=$DEFAULT_PROMETHEUS_PORT
PROFILES=""
ENV_FILE=""
CONFIG_FILE=""

# コマンドライン引数の解析
while [[ $# -gt 0 ]]; do
    case $1 in
        -r|--rest-port)
            REST_API_PORT="$2"
            shift 2
            ;;
        -t|--traefik-port)
            TRAEFIK_PORT="$2"
            shift 2
            ;;
        -d|--dashboard-port)
            TRAEFIK_DASHBOARD_PORT="$2"
            shift 2
            ;;
        -p|--prometheus-port)
            PROMETHEUS_PORT="$2"
            shift 2
            ;;
        --config)
            CONFIG_FILE="$2"
            shift 2
            ;;
        --production)
            PROFILES="$PROFILES --profile production"
            shift
            ;;
        --monitoring)
            PROFILES="$PROFILES --profile monitoring"
            shift
            ;;
        --env-file)
            ENV_FILE="$2"
            shift 2
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            echo "エラー: 不明なオプション $1"
            show_help
            exit 1
            ;;
    esac
done

# 設定の表示
echo "=== md-roam-server 起動設定 ==="
echo "REST API ポート: $REST_API_PORT"
echo "Traefik ポート: $TRAEFIK_PORT"
echo "Traefik ダッシュボード ポート: $TRAEFIK_DASHBOARD_PORT"
echo "Prometheus ポート: $PROMETHEUS_PORT"
if [[ -n "$CONFIG_FILE" ]]; then
    echo "設定ファイル: $CONFIG_FILE"
fi
if [[ -n "$PROFILES" ]]; then
    echo "プロファイル: $PROFILES"
fi
if [[ -n "$ENV_FILE" ]]; then
    echo "環境変数ファイル: $ENV_FILE"
fi
echo "================================"

# 環境変数の設定
export REST_API_PORT
export TRAEFIK_PORT
export TRAEFIK_DASHBOARD_PORT
export PROMETHEUS_PORT

# 設定ファイルの環境変数を設定
if [[ -n "$CONFIG_FILE" ]]; then
    export MD_ROAM_CONFIG_FILE="$CONFIG_FILE"
fi

# docker-composeコマンドの構築
COMPOSE_CMD="docker-compose"

if [[ -n "$ENV_FILE" ]]; then
    COMPOSE_CMD="$COMPOSE_CMD --env-file $ENV_FILE"
fi

COMPOSE_CMD="$COMPOSE_CMD$PROFILES up -d"

echo "実行コマンド: $COMPOSE_CMD"
echo ""

# 実行
eval $COMPOSE_CMD

echo ""
echo "✅ md-roam-server が起動しました！"
echo ""
echo "アクセス先:"
echo "  REST API: http://localhost:$REST_API_PORT"
if [[ "$PROFILES" == *"production"* ]]; then
    echo "  Traefik: http://localhost:$TRAEFIK_PORT"
    echo "  Traefik ダッシュボード: http://localhost:$TRAEFIK_DASHBOARD_PORT"
fi
if [[ "$PROFILES" == *"monitoring"* ]]; then
    echo "  Prometheus: http://localhost:$PROMETHEUS_PORT"
fi
