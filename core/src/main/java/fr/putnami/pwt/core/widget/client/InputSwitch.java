package fr.putnami.pwt.core.widget.client;

import com.google.common.collect.Maps;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.user.client.ui.IsWidget;

import java.util.Map;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractInputChoice;
import fr.putnami.pwt.core.widget.client.base.AbstractInputSelect;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class InputSwitch<T> extends AbstractInputChoice<T, T> {

	private static final CssStyle STYLE_CHOICE_SLIDER = new SimpleStyle("input-switch");
	private static final CssStyle STYLE_ITEM = new SimpleStyle("switch-item");
	private static final CssStyle STYLE_CHOICE_ITEM_SLIDER = new SimpleStyle("switch-item-slider");
	private static final CssStyle STYLE_ACTIVE = new SimpleStyle("switch-item-active");

	private class SwitchHandler implements ChoiceSelectionHandler<T, T> {
		private T selectedItem;

		@Override
		public void onItemClick(T item) {
			selectedItem = item;
			ValueChangeEvent.fire(InputSwitch.this, selectedItem);
			resetSlider(item);
		}

		@Override
		public void setSelection(T selection, boolean fireEvents) {
			T oldValue = selectedItem;
			resetSlider(selection);
			selectedItem = selection;
			if (fireEvents) {
				ValueChangeEvent.fireIfNotEqual(InputSwitch.this, oldValue, selection);
			}
		}

		@Override
		public T getSelection() {
			return selectedItem;
		}
	}

	private class SwitchItem extends Container implements ClickHandler {

		private T value;

		public SwitchItem(T value) {
			this.value = value;
			StyleUtils.addStyle(this, STYLE_ITEM);
			getElement().setInnerHTML(getChoiceRenderer().renderItem(value));
			addDomHandler(this, ClickEvent.getType());
		}

		@Override
		public void onClick(ClickEvent event) {
			getSelectionHandler().onItemClick(value);
		}
	}

	private ChoiceSelectionHandler<T, T> selectionHandler = new SwitchHandler();
	private ChoiceRenderer<T> choiceRenderer = new ChoiceRendererImpl();

	private final Container container;
	private final Container slider;
	private final Map<T, SwitchItem> items = Maps.newHashMap();

	public InputSwitch() {
		super(new Container());
		container = (Container) getWidget();
		endConstruct();
		slider = new Container();
		StyleUtils.addStyle(slider, STYLE_CHOICE_ITEM_SLIDER);
		StyleUtils.addStyle(this, STYLE_CHOICE_SLIDER);
	}

	protected InputSwitch(AbstractInputSelect<T, T> source) {
		super(new Container(), source);
		container = (Container) getWidget();
		slider = new Container();
		StyleUtils.addStyle(slider, STYLE_CHOICE_ITEM_SLIDER);
		endConstruct();
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputSwitch<T>();
	}

	@Override
	protected void redrawInternal() {
		container.clear();
		items.clear();
		container.add(slider);

		for (T item : getOrderedItems()) {
			SwitchItem choiceItem = new SwitchItem(item);
			container.add(choiceItem);
			items.put(item, choiceItem);
		}
		resetSlider(getValue());
	}

	@Override
	protected ChoiceRenderer<T> getChoiceRenderer() {
		return choiceRenderer;
	}

	@Override
	protected void setChoiceRenderer(ChoiceRenderer<T> choiceRenderer) {
		this.choiceRenderer = choiceRenderer;
	}

	@Override
	protected ChoiceSelectionHandler<T, T> getSelectionHandler() {
		return selectionHandler;
	}

	@Override
	protected void setSelectionHandler(ChoiceSelectionHandler<T, T> selectionHandler) {
		this.selectionHandler = selectionHandler;
	}

	private void resetSlider(T value) {
		for (SwitchItem item : items.values()) {
			StyleUtils.removeStyle(item, STYLE_ACTIVE);
		}
		final SwitchItem item = items.get(value);
		if (item != null) {

			StyleUtils.addStyle(item, STYLE_ACTIVE);
			Scheduler.get().scheduleDeferred(new ScheduledCommand() {
				@Override
				public void execute() {
					Element itemElement = item.getElement();
					slider.getElement().getStyle().setWidth(itemElement.getClientWidth(), Unit.PX);
					slider.getElement().getStyle().setMarginLeft(itemElement.getOffsetLeft() - 5, Unit.PX);
				}
			});
		}
	}

}
