import redis.asyncio as redis
from monitoramento_indiretos.core.config import settings

redis_client = redis.from_url(
    f"redis://{settings.REDIS_HOST}:{settings.REDIS_PORT}/{settings.REDIS_DB}",
    decode_responses=True,
)