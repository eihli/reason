import logging
import logging.config
import os

LOG_FORMAT = "%(asctime)s %(pathname)s:%(lineno)d %(funcName)s %(levelname)s: %(message)s"

log_level = getattr(logging, os.environ.get('REASON_LOG_LVL', 'WARNING').upper())
logging_config = {
    'version': 1,
    'formatters': {
        'standard': {
            'format': LOG_FORMAT,
        },
    },
    'handlers': {
        'default': {
            'level': log_level,
            'formatter': 'standard',
            'class': 'logging.StreamHandler',
        },
    },
    'loggers': {
        '': {
            'handlers': ['default'],
            'level': log_level,
            'propagate': True,
        },
    },
}

logging.config.dictConfig(logging_config)
