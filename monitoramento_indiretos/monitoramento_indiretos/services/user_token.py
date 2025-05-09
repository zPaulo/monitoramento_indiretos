from monitoramento_indiretos.core.redis import redis_client

REDIS_PREFIX = "visipec"

async def store_token(email: str, token: str, ttl: int):
    key = f"{REDIS_PREFIX}:{email}"
    await redis_client.set(key, token, ex=ttl)

async def fetch_token(email: str) -> str:
    return await redis_client.get(f"{REDIS_PREFIX}:{email}")

async def delete_token(email: str):
    await redis_client.delete(f"{REDIS_PREFIX}:{email}")