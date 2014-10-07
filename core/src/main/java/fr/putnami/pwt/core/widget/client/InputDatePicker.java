/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client;

import com.google.common.base.Objects;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.LIElement;
import com.google.gwt.dom.client.Style;
import com.google.gwt.dom.client.Style.Display;
import com.google.gwt.dom.client.Style.Position;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.dom.client.TableCellElement;
import com.google.gwt.dom.client.TableElement;
import com.google.gwt.dom.client.TableRowElement;
import com.google.gwt.dom.client.TableSectionElement;
import com.google.gwt.dom.client.UListElement;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.LocaleInfo;
import com.google.gwt.i18n.shared.DateTimeFormatInfo;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.ui.FocusWidget;
import com.google.gwt.user.client.ui.HasValue;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.Widget;
import com.google.gwt.user.datepicker.client.CalendarUtil;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.EditorLeaf;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.util.ValidationUtils;
import fr.putnami.pwt.core.editor.client.validator.Validator;
import fr.putnami.pwt.core.event.client.EventBus;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.model.client.base.HasHtmlId;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.constant.WidgetParams;
import fr.putnami.pwt.core.widget.client.event.AskFocusEvent;
import fr.putnami.pwt.core.widget.client.util.KeyEventUtils;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class InputDatePicker extends FocusWidget
	implements EditorLeaf, EditorInput<Date>, HasHtmlId, HasDrawable, HasValue<Date> {

	private static final CssStyle STYLE_DATEPICKER = new SimpleStyle("datepicker");
	private static final CssStyle STYLE_POPUP = new SimpleStyle("datepicker-popup");
	private static final CssStyle STYLE_HEADER = new SimpleStyle("datepicker-header");
	private static final CssStyle STYLE_MONTH_PICKER_BUTTON = new SimpleStyle("month-picker-button");
	private static final CssStyle STYLE_MONTH_PICKER = new SimpleStyle("month-picker");
	private static final CssStyle STYLE_MONTH_PAGER = new SimpleStyle("month-pager");
	private static final CssStyle STYLE_MONTH_PREVIOUS = new SimpleStyle("month-previous");
	private static final CssStyle STYLE_MONTH_NEXT = new SimpleStyle("month-next");
	private static final CssStyle STYLE_YEAR_BUTTON = new SimpleStyle("year-button");
	private static final CssStyle STYLE_CALENDAR_PICKER = new SimpleStyle("calendar-picker");
	private static final CssStyle STYLE_MUTED = new SimpleStyle("muted");
	private static final CssStyle STYLE_SELECTED = new SimpleStyle("selected");
	private static final CssStyle STYLE_TODAY = new SimpleStyle("today");
	private static final CssStyle STYLE_SHOW = new SimpleStyle("in");

	public enum Mode {
			CALENDAR,
			MONTH;
	}

	private static final WidgetParams WIDGET_PARAMS = WidgetParams.Util.get();

	private static final CssStyle STYLE_FADE = new SimpleStyle("fade");

	private static final String ATTRIBUTE_DATA_DATE = "data-date";
	private static final String ATTRIBUTE_DATA_CURSOR = "data-cursor";
	private static final String ATTRIBUTE_DATA_YEAR = "data-year";

	private static final int DAYS_IN_WEEK = 7;

	private static final int YEAR_OFFSET = 1900;

	private static final DateTimeFormatInfo DATE_TIME_FORMAT_INFO = LocaleInfo.getCurrentLocale()
		.getDateTimeFormatInfo();
	private static final String[] DAYS = InputDatePicker.DATE_TIME_FORMAT_INFO
		.weekdaysShortStandalone();

	private static final DateTimeFormat MONTH_YEAR_FORMAT = DateTimeFormat
		.getFormat(InputDatePicker.WIDGET_PARAMS
			.inputDatePickerMonthYearFormat());
	private static final DateTimeFormat MONTH_ABBR_FORMAT = DateTimeFormat
		.getFormat(InputDatePicker.WIDGET_PARAMS
			.inputDatePickerMonthFormat());

	private static final DateTimeFormat ATTRIBUTE_DATE_FORMAT = DateTimeFormat
		.getFormat("yyyy-MM-dd");

	private final DivElement datepickerHeader = Document.get().createDivElement();

	private final DivElement monthPickerButton = Document.get().createDivElement();

	private final UListElement monthPagerUl = Document.get().createULElement();
	private final LIElement pagePreviusMonthLi = Document.get().createLIElement();
	private final LIElement pageTodayLi = Document.get().createLIElement();
	private final LIElement pageNextMonthLi = Document.get().createLIElement();

	private final TableElement calendarTable = Document.get().createTableElement();
	private final TableSectionElement calendatBody = Document.get().createTBodyElement();

	private final DivElement monthPicker = Document.get().createDivElement();
	private final DivElement monthPickerInner = Document.get().createDivElement();
	private final UListElement monthPickerUlMonthElement = Document.get().createULElement();

	private HandlerRegistration popupBlurHandler;

	private Mode mode = Mode.CALENDAR;

	private Date today;
	private Date cursor;
	private Date value;

	private String path;

	private List<Error> errors = Lists.newArrayList();
	private Collection<Validator<Date>> validators;
	private String htmlId;
	private com.google.web.bindery.event.shared.HandlerRegistration askFocusRegistration;

	public InputDatePicker() {
		super(Document.get().createDivElement());

		StyleUtils.addStyle(this, InputDatePicker.STYLE_DATEPICKER);

		this.getElement().appendChild(this.datepickerHeader);
		StyleUtils.addStyle(this.datepickerHeader, InputDatePicker.STYLE_HEADER);

		/* month selecter */
		this.datepickerHeader.appendChild(this.monthPickerButton);
		StyleUtils.addStyle(this.monthPickerButton, InputDatePicker.STYLE_MONTH_PICKER_BUTTON);

		/* pagination */
		this.datepickerHeader.appendChild(this.monthPagerUl);
		StyleUtils.addStyle(this.monthPagerUl, InputDatePicker.STYLE_MONTH_PAGER);
		this.createLi(this.pagePreviusMonthLi, InputDatePicker.STYLE_MONTH_PREVIOUS, "&#9668;");
		this.createLi(this.pageTodayLi, InputDatePicker.STYLE_TODAY, "&#9673;");
		this.createLi(this.pageNextMonthLi, InputDatePicker.STYLE_MONTH_NEXT, "&#9658;");

		this.monthPagerUl.appendChild(this.pagePreviusMonthLi);
		this.monthPagerUl.appendChild(this.pageTodayLi);
		this.monthPagerUl.appendChild(this.pageNextMonthLi);

		/* Calendar Picker */
		this.getElement().appendChild(this.monthPicker);
		this.monthPicker.appendChild(this.monthPickerInner);
		StyleUtils.addStyle(this.monthPicker, InputDatePicker.STYLE_MONTH_PICKER);

		/* Calendar Picker */
		this.getElement().appendChild(this.calendarTable);
		StyleUtils.addStyle(this.calendarTable, InputDatePicker.STYLE_CALENDAR_PICKER);

		/* DayPicker Header */
		TableSectionElement head = Document.get().createTHeadElement();
		TableRowElement headRow = Document.get().createTRElement();
		this.calendarTable.appendChild(head);
		head.appendChild(headRow);
		for (int i = 0; i < 7; i++) {
			TableCellElement th = Document.get().createTHElement();
			headRow.appendChild(th);
			int dayToDisplay =
				(i + InputDatePicker.DATE_TIME_FORMAT_INFO.firstDayOfTheWeek())
					% InputDatePicker.DAYS.length;
			th.setInnerText(InputDatePicker.DAYS[dayToDisplay]);
		}
		/* DayPicker Body */
		this.calendarTable.appendChild(this.calendatBody);

		this.today =
			InputDatePicker.ATTRIBUTE_DATE_FORMAT.parse(InputDatePicker.ATTRIBUTE_DATE_FORMAT
				.format(new Date()));
		this.setValue(this.today);

		Event.sinkEvents(this.getElement(), Event.ONKEYDOWN);
		Event.sinkEvents(this.monthPickerButton, Event.ONCLICK);
		Event.sinkEvents(this.pagePreviusMonthLi, Event.ONCLICK);
		Event.sinkEvents(this.pageTodayLi, Event.ONCLICK);
		Event.sinkEvents(this.pageNextMonthLi, Event.ONCLICK);

		this.redraw();
	}

	public InputDatePicker(InputDatePicker source) {
		this();
		this.today = source.today;
		this.cursor = source.cursor;
		this.path = source.path;
		this.validators = source.validators;
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputDatePicker(this);
	}

	@Override
	public HandlerRegistration addValueChangeHandler(ValueChangeHandler<Date> handler) {
		return this.addHandler(handler, ValueChangeEvent.getType());
	}

	@Override
	public Date getValue() {
		return this.value == null ? null : new Date(this.value.getTime());
	}

	@Override
	public void setValue(Date value) {
		this.setValue(value, false);
	}

	@Override
	public void setValue(Date value, boolean fireEvents) {
		if (fireEvents) {
			ValueChangeEvent.fireIfNotEqual(this, this.value, value);
			this.setFocus(true);
		}
		if (value == null) {
			this.value = null;
			this.cursor = new Date(this.today.getTime());
		} else {
			this.value = new Date(value.getTime());
			this.cursor = new Date(value.getTime());
		}
		this.redraw();
	}

	@Override
	public void onBrowserEvent(Event event) {
		int type = event.getTypeInt();
		Element target = event.getCurrentTarget();
		if (type == Event.ONCLICK) {
			String dataDate = target.getAttribute(InputDatePicker.ATTRIBUTE_DATA_DATE);
			String dataCursor = target.getAttribute(InputDatePicker.ATTRIBUTE_DATA_CURSOR);
			String dataYear = target.getAttribute(InputDatePicker.ATTRIBUTE_DATA_YEAR);
			if (dataDate != null && dataDate.length() > 0) {
				this.mode = Mode.CALENDAR;
				this.setValue(InputDatePicker.ATTRIBUTE_DATE_FORMAT.parse(dataDate), true);
			} else if (dataCursor != null && dataCursor.length() > 0) {
				this.mode = Mode.CALENDAR;
				this.cursor = InputDatePicker.ATTRIBUTE_DATE_FORMAT.parse(dataCursor);
				this.redraw();
			} else if (dataYear != null && dataYear.length() > 0) {
				this.openMonthOfYear(Integer.valueOf(dataYear));
			} else if (target == this.monthPickerButton) {
				if (this.mode != Mode.MONTH) {
					this.mode = Mode.MONTH;
				} else {
					this.mode = Mode.CALENDAR;
				}
				this.redraw();
			}
			event.stopPropagation();
			event.preventDefault();
		} else if (type == Event.ONKEYDOWN) {
			this.handleKeyPress(event.getKeyCode());
			event.stopPropagation();
			event.preventDefault();
		} else {
			super.onBrowserEvent(event);
		}
	}

	@Override
	public void redraw() {
		if (this.mode == Mode.CALENDAR) {
			this.redrawCalendarPicker();
		} else if (this.mode == Mode.MONTH) {
			this.redrawMonthPicker();
		}
	}

	private void redrawMonthPicker() {
		this.monthPicker.getStyle().setWidth(this.calendarTable.getClientWidth(), Unit.PX);
		this.calendarTable.getStyle().setDisplay(Display.NONE);
		this.monthPicker.getStyle().clearDisplay();

		int currentYear = this.cursor.getYear() + InputDatePicker.YEAR_OFFSET;
		if (this.monthPickerInner.getChildCount() == 0) {
			for (int year = currentYear - 100; year < currentYear + 100; year++) {
				DivElement yearDiv = Document.get().createDivElement();
				yearDiv.setInnerText("" + year);
				StyleUtils.addStyle(yearDiv, InputDatePicker.STYLE_YEAR_BUTTON);
				Event.sinkEvents(yearDiv, Event.ONCLICK);
				this.monthPickerInner.appendChild(yearDiv);
				yearDiv.setAttribute(InputDatePicker.ATTRIBUTE_DATA_YEAR, "" + year);
			}
		}
		this.openMonthOfYear(this.cursor.getYear() + InputDatePicker.YEAR_OFFSET);
	}

	private void openMonthOfYear(int year) {
		String yearString = "" + year;
		this.monthPickerUlMonthElement.removeFromParent();
		for (int i = 0; i < this.monthPickerInner.getChildCount(); i++) {
			Element child = (Element) this.monthPickerInner.getChild(i);
			if (yearString.equals(child.getAttribute(InputDatePicker.ATTRIBUTE_DATA_YEAR))) {
				this.monthPickerInner.insertAfter(this.monthPickerUlMonthElement, child);
				Date monthButtonDate = new Date(this.cursor.getTime());
				monthButtonDate.setYear(year - InputDatePicker.YEAR_OFFSET);
				if (this.monthPickerUlMonthElement.getChildCount() == 0) {
					for (int month = 0; month < 12; month++) {
						LIElement monthElement = Document.get().createLIElement();
						this.monthPickerUlMonthElement.appendChild(monthElement);
						Event.sinkEvents(monthElement, Event.ONCLICK);
						monthButtonDate.setMonth(month);
						monthElement.setInnerText(InputDatePicker.MONTH_ABBR_FORMAT.format(monthButtonDate));
					}
				}
				for (int month = 0; month < 12; month++) {
					LIElement monthElement = (LIElement) this.monthPickerUlMonthElement.getChild(month);
					monthButtonDate.setMonth(month);
					monthElement.setAttribute(InputDatePicker.ATTRIBUTE_DATA_CURSOR,
						InputDatePicker.ATTRIBUTE_DATE_FORMAT
							.format(monthButtonDate));
				}
				this.monthPicker.setScrollTop(child.getOffsetTop());
				break;
			}
		}
	}

	private void redrawCalendarPicker() {
		this.monthPicker.getStyle().setDisplay(Display.NONE);
		this.calendarTable.getStyle().clearDisplay();

		this.calendatBody.removeAllChildren();

		int firstDayOfWeek = InputDatePicker.DATE_TIME_FORMAT_INFO.firstDayOfTheWeek();
		int lastDayOfWeek =
			(firstDayOfWeek + InputDatePicker.DAYS_IN_WEEK) % InputDatePicker.DAYS_IN_WEEK;

		/* Display month */
		this.monthPickerButton.setInnerHTML(InputDatePicker.MONTH_YEAR_FORMAT.format(this.cursor)
			+ "<span class=\"caret\"></span>");

		Date lastMonth = new Date(this.cursor.getTime());
		CalendarUtil.addMonthsToDate(lastMonth, -1);
		this.pagePreviusMonthLi.setAttribute(InputDatePicker.ATTRIBUTE_DATA_CURSOR,
			InputDatePicker.ATTRIBUTE_DATE_FORMAT
				.format(lastMonth));
		this.pageTodayLi.setAttribute(InputDatePicker.ATTRIBUTE_DATA_CURSOR,
			InputDatePicker.ATTRIBUTE_DATE_FORMAT
				.format(this.today));
		Date nextMonth = new Date(this.cursor.getTime());
		CalendarUtil.addMonthsToDate(nextMonth, 1);
		this.pageNextMonthLi.setAttribute(InputDatePicker.ATTRIBUTE_DATA_CURSOR,
			InputDatePicker.ATTRIBUTE_DATE_FORMAT
				.format(nextMonth));

		/* Draw daypicker */
		Date dateToDrow = new Date(this.cursor.getTime());
		int selectedMonth = dateToDrow.getMonth();
		int firstMonthToDisplay = (selectedMonth + 11) % 12;
		int lastMonthToDisplay = (selectedMonth + 13) % 12;
		do {
			CalendarUtil.addDaysToDate(dateToDrow, -1);
		} while (firstMonthToDisplay != dateToDrow.getMonth() || dateToDrow.getDay() != firstDayOfWeek);

		// drow calendarTable
		TableRowElement headRow = null;
		while (dateToDrow.getMonth() != lastMonthToDisplay || dateToDrow.getDay() != lastDayOfWeek
			|| dateToDrow.getDate() == 1 && dateToDrow.getDay() == firstDayOfWeek) {
			if (headRow == null || dateToDrow.getDay() == firstDayOfWeek) {
				headRow = Document.get().createTRElement();
				this.calendatBody.appendChild(headRow);
			}
			TableCellElement td = Document.get().createTDElement();
			headRow.appendChild(td);
			DivElement div = Document.get().createDivElement();
			td.appendChild(div);
			div.setInnerText("" + dateToDrow.getDate());
			div.setAttribute(InputDatePicker.ATTRIBUTE_DATA_DATE, InputDatePicker.ATTRIBUTE_DATE_FORMAT
				.format(dateToDrow));
			if (dateToDrow.getMonth() != selectedMonth) {
				StyleUtils.addStyle(td, InputDatePicker.STYLE_MUTED);
			}
			if (dateToDrow.equals(this.cursor)) {
				StyleUtils.addStyle(td, InputDatePicker.STYLE_SELECTED);
			}
			if (this.today.equals(dateToDrow)) {
				StyleUtils.addStyle(td, InputDatePicker.STYLE_TODAY);
			}
			Event.sinkEvents(div, Event.ONCLICK);

			CalendarUtil.addDaysToDate(dateToDrow, 1);
		}
	}

	private boolean handleKeyPress(int keyCode) {
		if (KeyEventUtils.isModifierKeyDown(Event.getCurrentEvent())) {
			return false;
		}
		boolean handleKey = false;
		switch (keyCode) {
			case KeyCodes.KEY_LEFT:
				CalendarUtil.addDaysToDate(this.cursor, -1);
				handleKey = true;
				break;
			case KeyCodes.KEY_RIGHT:
				CalendarUtil.addDaysToDate(this.cursor, 1);
				handleKey = true;
				break;
			case KeyCodes.KEY_UP:
				CalendarUtil.addDaysToDate(this.cursor, -7);
				handleKey = true;
				break;
			case KeyCodes.KEY_DOWN:
				CalendarUtil.addDaysToDate(this.cursor, 7);
				handleKey = true;
				break;
			case KeyCodes.KEY_PAGEUP:
				CalendarUtil.addMonthsToDate(this.cursor, -1);
				handleKey = true;
				break;
			case KeyCodes.KEY_PAGEDOWN:
				CalendarUtil.addMonthsToDate(this.cursor, 1);
				handleKey = true;
				break;
			case KeyCodes.KEY_ENTER:
				this.setValue(this.cursor, true);
				handleKey = true;
				break;
			case KeyCodes.KEY_ESCAPE:
				this.setValue(this.value);
				this.setFocus(false);
				handleKey = true;
				break;
			default:
				break;
		}
		if (handleKey) {
			this.redraw();
		}

		return handleKey;
	}

	private void createLi(LIElement liElement, CssStyle style, String text) {
		StyleUtils.addStyle(liElement, style);
		liElement.setInnerHTML(text);
	}

	public void popup(Widget container, Widget relativeTo) {
		this.setVisible(true);
		StyleUtils.addStyle(this, InputDatePicker.STYLE_POPUP);
		RootPanel.get().add(this);

		Element positioningElement = this.getElement();
		Element relativeElement = relativeTo.getElement();

		int targetHeight = relativeElement.getOffsetHeight();
		int targetTop = relativeElement.getAbsoluteTop();

		int positioningWidth = positioningElement.getOffsetWidth();
		int targetRight = relativeElement.getAbsoluteRight();

		Style elementStyle = positioningElement.getStyle();
		elementStyle.setPosition(Position.ABSOLUTE);
		elementStyle.setLeft(targetRight - positioningWidth, Unit.PX);
		elementStyle.setTop(targetTop + targetHeight, Unit.PX);

		StyleUtils.addStyle(this, InputDatePicker.STYLE_FADE);
		StyleUtils.addStyle(this, InputDatePicker.STYLE_SHOW);

		this.setFocus(true);

		if (this.popupBlurHandler == null) {
			this.popupBlurHandler = this.addBlurHandler(new BlurHandler() {

				@Override
				public void onBlur(BlurEvent event) {
					InputDatePicker.this.hide();
				}
			});
		}
	}

	public void hide() {
		StyleUtils.removeStyle(this.getElement(), InputDatePicker.STYLE_SHOW);
		Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

			@Override
			public boolean execute() {
				InputDatePicker.this.setVisible(false);
				RootPanel.get().remove(InputDatePicker.this);
				return false;
			}
		}, 200);
	}

	public void togglePopup(Widget container, Widget relativeTo) {
		if (!this.isVisible() || !this.isAttached()) {
			this.popup(container, relativeTo);
		} else {
			this.hide();
		}
	}

	@Override
	public String getPath() {
		return this.path;
	}

	@Override
	public Date flush() {
		this.errors.clear();
		this.errors.addAll(ValidationUtils.validate(this.validators, this, this.value));
		return this.getValue();
	}

	@Override
	public void edit(Date value) {
		this.setValue(value);
	}

	@Override
	public boolean hasErrors() {
		return !this.errors.isEmpty();
	}

	@Override
	public Iterable<Error> getErrors() {
		return Iterables.unmodifiableIterable(this.errors);
	}

	@Override
	public void addValidator(Validator<Date> validator) {
		if (this.validators == null) {
			this.validators = Lists.newArrayList();
		}
		this.validators.add(validator);
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public void setHtmlId(String htmlId) {
		this.htmlId = htmlId;
		if (this.askFocusRegistration != null) {
			this.askFocusRegistration.removeHandler();
		}
		if (htmlId != null) {
			AskFocusEvent.Handler handler = new AskFocusEvent.Handler() {

				@Override
				public void onAskFocus(AskFocusEvent event) {
					if (Objects.equal(event.getHtmlId(), InputDatePicker.this.htmlId)) {
						InputDatePicker.this.setFocus(true);
					}
				}
			};
			this.askFocusRegistration = EventBus.get().addHandler(AskFocusEvent.TYPE, handler);
		}
	}

	@Override
	public String getHtmlId() {
		return this.htmlId;
	}

}
