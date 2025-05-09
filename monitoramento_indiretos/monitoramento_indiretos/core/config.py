from decouple import config
from pydantic_settings import BaseSettings, SettingsConfigDict
from functools import lru_cache

class Settings(BaseSettings):
    # Default do projeto
    PROJECT_NAME: str

    # Redis
    REDIS_HOST: str
    REDISPORT: int
    REDIS_DB: int

    # JWT
    JWT_SECRET_KEY: str
    JWT_ALGORITHM: str
    JWT_EXPIRATION: int

    # URL de APIs
    FRONTEND_HOST: str 
    VISIPEC_URL_LOGIN: str
    VISIPEC_URL_AUTH: str
    
    model_config = SettingsConfigDict(
        env_file="../.env",
        env_ignore_empty=True,
        extra="ignore")

settings = Settings()

@lru_cache
def get_settings():  # único objeto Settings durante todo o processo
    return Settings()