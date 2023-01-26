from unittest.mock import patch
from unittest import TestCase

from client import VkClient
from stats import StatisticsService


class StatisticsServiceTest(TestCase):
    @patch.object(VkClient, "search_feed")
    def test_get_posts_success(self, mock_search_feed):
        mock_search_feed.return_value = {"total_count": 228}
        stats_service = StatisticsService()
        expected = [228]
        actual = stats_service.get_posts("query", 1)
        self.assertEqual(expected, actual)

    def test_get_posts_hours_out_of_range(self):
        stats_service = StatisticsService()
        with self.assertRaises(ValueError):
            stats_service.get_posts("query", 228)
