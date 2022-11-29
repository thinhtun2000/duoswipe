import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { User } from 'src/app/core/models/user';

@Component({
  selector: 'app-user-preferences-card',
  templateUrl: './user-preferences-card.component.html',
  styleUrls: ['./user-preferences-card.component.scss'],
})
export class UserPreferencesCardComponent implements OnInit {
  public form: FormGroup;
  @Input() user: User | null;
  @Output() prefEmitter: EventEmitter<any> = new EventEmitter<any>();

  constructor(private fb: FormBuilder) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      user_id: [''],
      name: [''],
      password: [''],
      email: [''],
      language_id: [''],
      location_id: [''],
      pref_pos: [''],
      pref_lang: [''],
      pref_day: [''],
      pref_time: [''],
      pos_1: [''],
      pos_2: [''],
      rank_id: [''],
    });
    if (this.user) this.form.patchValue(this.user);
  }

  public pref_submit() {
    const user = new User(this.form.getRawValue());
    this.prefEmitter.emit({ type: 'pref', data: user });
  }
}
