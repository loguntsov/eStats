

-record(click_info,{
  id :: integer(), % ID клика INT
  transaction_id:: binary(), % ID клика string
  offer_id :: integer(), % ID кампании
  offer_url_id :: integer(), % ID урла по которму был переход (у одной кампании может быть несколько урлов на разные лендинги)
  % banner id (не реализовано)
  affiliate_id :: integer(), % ID аффила
  % advertiser_id :: integer(), % ID рекламодателя
  subid :: dict(),  % Список subid
  http_referer :: binary(),
  domain :: binary(),
  user_agent :: binary(),
  ip :: binary(),
  year :: integer(),
  month :: integer(),
  day :: integer(),
  hour :: integer()
}).

