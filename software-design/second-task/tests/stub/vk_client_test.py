from stubserver import StubServer
from unittest import TestCase
import os
import json
from client import VkClient


class VkClientTest(TestCase):
    def setUp(self):
        port = 8998
        os.environ["VK_CLIENT_URL"] = "http://localhost:{}".format(port)
        self.server = StubServer(port)
        self.server.run()

    def tearDown(self):
        self.server.stop()

    def test_newsfeed_search(self):
        dummy_response = json.dumps({"response": {"total_count": 228}})
        capture = {}

        self.server.expect(
            method="GET", url="/method/newsfeed.search", data_capture=capture
        ).and_return(reply_code=201, content=dummy_response)

        client = VkClient()
        result = client.search_feed(payload={})

        self.assertEqual(228, result["total_count"])
