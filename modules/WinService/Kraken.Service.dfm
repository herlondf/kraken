object KrakenService: TKrakenService
  OldCreateOrder = False
  DisplayName = 'KrakenService'
  AfterInstall = ServiceAfterInstall
  OnExecute = ServiceExecute
  Height = 176
  Width = 311
end
