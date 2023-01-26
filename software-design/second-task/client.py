import requests
import os


class VkClient:
    def __init__(self) -> None:
        self.__url = os.environ.get("VK_CLIENT_URL")
        access_token = os.environ.get("VK_CLIENT_ACCESS_TOKEN")
        api_version = os.environ.get("VK_CLIENT_API_VERSION")

        self.__default_parameters = {
            "access_token": access_token,
            "v": api_version,
        }

    def search_feed(self, payload):
        request = {**self.__default_parameters, **payload}
        response = requests.get(
            "{}/method/newsfeed.search".format(self.__url), params=request
        )
        return response.json()["response"]
