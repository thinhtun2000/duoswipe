import { TestBed } from '@angular/core/testing';

import { SwipeService } from './swipe.service';

describe('SwipeService', () => {
  let service: SwipeService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(SwipeService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
