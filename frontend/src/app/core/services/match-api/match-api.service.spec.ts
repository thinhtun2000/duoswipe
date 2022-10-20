import { TestBed } from '@angular/core/testing';

import { MatchApiService } from './match-api.service';

describe('MatchApiService', () => {
  let service: MatchApiService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(MatchApiService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
