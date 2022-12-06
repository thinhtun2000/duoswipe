import {
  AfterContentInit,
  AfterViewInit,
  Component,
  EventEmitter,
  Input,
  OnInit,
  Output,
} from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { User } from 'src/app/core/models/user';

@Component({
  selector: 'app-user-info-card',
  templateUrl: './user-info-card.component.html',
  styleUrls: ['./user-info-card.component.scss'],
})
export class UserInfoCardComponent implements OnInit {
  public form: FormGroup;
  @Input() user: User | null;
  @Input() disable: boolean;
  @Output() profileEmitter: EventEmitter<any> = new EventEmitter<any>();

  constructor(private fb: FormBuilder) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      user_id: [''],
      name: [''],
      password: [''],
      email: [''],
      language: [''],
      location: [''],
      pref_pos: [''],
      pref_lang: [''],
      pref_day: [''],
      pref_time: [''],
      pos_1: [''],
      pos_2: [''],
      rank: [''],
    });
    if (this.user) this.form.patchValue(this.user);
  }

  public profile_submit() {
    const user = new User(this.form.getRawValue());
    this.profileEmitter.emit({ type: 'info', data: user });
  }
}
