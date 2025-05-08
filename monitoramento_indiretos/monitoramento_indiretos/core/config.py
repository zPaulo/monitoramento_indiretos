from decouple import config
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    API_V1_STR: str = "/api/v1"
    PROJECT_NAME: str = "Monitoramento Indiretos"
    FRONTEND_HOST: str = config("FRONTEND_HOST", default="http://localhost:3000")

settings = Settings()