/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client.base;

import com.google.common.base.Predicate;
import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.text.shared.Renderer;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Event;

import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;

import fr.putnami.pwt.core.model.client.base.HasPlaceholder;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.ButtonDropdown;
import fr.putnami.pwt.core.widget.client.DropdownHeader;
import fr.putnami.pwt.core.widget.client.NavLink;
import fr.putnami.pwt.core.widget.client.helper.CompositeFocusHelper;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractInputSelect<T, U> extends AbstractInputChoice<T, U> implements
HasPlaceholder {

  private static final CssStyle STYLE_INPUT_SELECT = new SimpleStyle("input-select");

  private class ItemClickCommand implements ScheduledCommand {
    private T item;

    public ItemClickCommand(T item) {
      this.item = item;
    }

    @Override
    public void execute() {
      getSelectionHandler().onItemClick(item);
      AbstractInputSelect.this.setFocus(true);
    }
  }

  public interface SelectItemSelectionHandler<T, U> extends ChoiceSelectionHandler<T, U> {

    void onHomeKeyDown();

    void onEndKeyDown();

    void onUpKeyDown();

    void onDownKeyDown();

    void onItemSearch(T searchResult);

  }

  public interface SelectRenderer<T, U> extends ChoiceRenderer<T> {

    Renderer<U> getSelectionRenderer();

    void setSelectionRenderer(Renderer<U> renderer);

    String renderSelection(U selection);

    String getPlaceholder();

    void setPlaceholder(String placeholder);

  }

  private class SelectRendererImpl extends ChoiceRendererImpl implements SelectRenderer<T, U> {

    private Renderer<U> selectionRenderer;
    private String placeholder;

    @Override
    public Renderer<U> getSelectionRenderer() {
      return selectionRenderer;
    }

    @Override
    public void setSelectionRenderer(Renderer<U> renderer) {
      this.selectionRenderer = renderer;
    }

    @Override
    public String renderSelection(U selection) {
      return render(selection, selectionRenderer);
    }

    @Override
    public String getPlaceholder() {
      return placeholder;
    }

    @Override
    public void setPlaceholder(String placeholder) {
      this.placeholder = placeholder;
    }

    @Override
    protected String renderNull() {
      if (!AbstractInputSelect.this.isNullValueAllowed()) {
        return this.placeholder != null ? this.placeholder : DEFAULT_NULL_RENDER;
      }
      return super.renderNull();
    }
  }

  private class KeyPressHandler implements Predicate<NavLink> {

    private static final long KEY_INTERVAL = 1200L;

    private String currentString = "";
    private long lastSearchTime = 0L;

    public void handleKeyPress(int charCode) {
      long currentTime = System.currentTimeMillis();
      if (currentTime - lastSearchTime > KEY_INTERVAL) {
        currentString = String.valueOf((char) charCode).toLowerCase();
      }
      else {
        currentString += String.valueOf((char) charCode).toLowerCase();
      }
      lastSearchTime = currentTime;
      NavLink matchingLink = Iterables.find(itemsLinks.values(), this, null);
      if (matchingLink != null) {
        selectionHandler.onItemSearch(itemsLinks.inverse().get(matchingLink));
      }
    }

    @Override
    public boolean apply(NavLink input) {
      return input.getLabel() != null && input.getLabel().toLowerCase().startsWith(currentString);
    }
  }

  private SelectRenderer<T, U> selectRenderer = new SelectRendererImpl();;
  private SelectItemSelectionHandler<T, U> selectionHandler;

  private final KeyPressHandler keyPressHandler = new KeyPressHandler();
  private CompositeFocusHelper compositeFocusHelper;

  private final Map<String, Collection<T>> itemsMap;
  private BiMap<T, NavLink> itemsLinks = HashBiMap.create();

  private final SimpleDropdown dropdown;

  public AbstractInputSelect() {
    super(new ButtonDropdown());
    itemsMap = Maps.newLinkedHashMap();
    dropdown = (SimpleDropdown) getWidget();

    endConstruct();
  }

  protected AbstractInputSelect(AbstractInputSelect<T, U> source) {
    super(new ButtonDropdown(), source);
    dropdown = (SimpleDropdown) getWidget();
    itemsMap = source.itemsMap;
    selectRenderer = source.selectRenderer;

    endConstruct();
  }

  @Override
  protected void endConstruct() {
    StyleUtils.removeStyle(this, ButtonDropdown.STYLE_BUTTON_GROUP);
    StyleUtils.addStyle(this, STYLE_INPUT_SELECT);

    compositeFocusHelper = CompositeFocusHelper.createFocusHelper(this, dropdown);
    dropdown.addAnchorStyle(STYLE_CONTROL);

    sinkEvents(Event.ONKEYDOWN | Event.ONKEYPRESS);

    super.endConstruct();
  }

  @Override
  public void setItems(Collection<T> items) {
    Map<String, Collection<T>> itemsMap = Maps.newHashMapWithExpectedSize(1);
    itemsMap.put(null, items);
    setItemsMap(itemsMap);
  }

  public void setItemsMap(Map<String, Collection<T>> itemsMap) {
    U currentValue = getValue();
    this.itemsMap.clear();
    this.itemsMap.putAll(itemsMap);
    edit(currentValue);
  }

  @Override
  protected void redrawInternal() {
    this.dropdown.clear();
    this.itemsLinks.clear();
    getOrderedItems().clear();

    if (isNullValueAllowed() && !isMultiple()) {
      addItem(null);
    }

    if (itemsMap != null) {
      for (Entry<String, Collection<T>> entry : itemsMap.entrySet()) {
        if (entry.getKey() != null) {
          dropdown.addMenuContent(new DropdownHeader(entry.getKey()));
        }
        for (T item : entry.getValue()) {
          addItem(item);
        }
      }
    }
  }

  public void addItem(T item) {
    NavLink link = new NavLink(getChoiceRenderer().renderItem(item), new ItemClickCommand(item));
    dropdown.addMenuContent(link);
    this.itemsLinks.put(item, link);
    getOrderedItems().add(item);
  }

  @Override
  public void setNullRender(String nullRender) {
    super.setNullRender(nullRender);
    renderLabel();
  }

  public void setSelectionRenderer(Renderer<U> renderer) {
    selectRenderer.setSelectionRenderer(renderer);
  }

  @Override
  public void setHtmlId(String htmlId) {
    super.setHtmlId(htmlId);
    dropdown.getAnchor().getElement().setId(htmlId);
  }

  @Override
  public String getPlaceholder() {
    return selectRenderer.getPlaceholder();
  }

  @Override
  public void setPlaceholder(String placeholder) {
    selectRenderer.setPlaceholder(placeholder);
    renderLabel();
  }

  @Override
  public void edit(U value) {
    super.edit(value);
    renderLabel();
  }

  private void renderLabel() {
    dropdown.setLabel(selectRenderer.renderSelection(getValue()));
  }

  @Override
  public void onValueChange(ValueChangeEvent<U> event) {
    if (event != null && event.getSource() == this) {
      dropdown.setLabel(selectRenderer.renderSelection(event.getValue()));
    }
  }

  @Override
  public void setFocus(boolean focused) {
    dropdown.setFocus(focused);
  }

  @Override
  public void setTabIndex(int index) {
    dropdown.setTabIndex(index);
  }

  @Override
  public int getTabIndex() {
    return dropdown.getTabIndex();
  }

  @Override
  public void setAccessKey(char key) {
    dropdown.setAccessKey(key);
  }

  @Override
  public com.google.gwt.event.shared.HandlerRegistration addBlurHandler(BlurHandler handler) {
    return compositeFocusHelper.addBlurHandler(handler);
  }

  @Override
  public com.google.gwt.event.shared.HandlerRegistration addFocusHandler(FocusHandler handler) {
    return compositeFocusHelper.addFocusHandler(handler);
  }

  @Override
  public void onBrowserEvent(Event event) {
    super.onBrowserEvent(event);
    boolean mustKillEvent = false;
    switch (DOM.eventGetType(event)) {
      case Event.ONKEYDOWN:
        switch (event.getKeyCode()) {
          case KeyCodes.KEY_HOME:
            selectionHandler.onHomeKeyDown();
            mustKillEvent = true;
            break;
          case KeyCodes.KEY_END:
            selectionHandler.onEndKeyDown();
            mustKillEvent = true;
            break;
          case KeyCodes.KEY_UP:
            selectionHandler.onUpKeyDown();
            mustKillEvent = true;
            break;
          case KeyCodes.KEY_DOWN:
            selectionHandler.onDownKeyDown();
            mustKillEvent = true;
            break;
          default:
            break;
        }
        break;
      case Event.ONKEYPRESS:
        keyPressHandler.handleKeyPress(event.getCharCode());
        break;
      default:
        break;
    }

    if (mustKillEvent) {
      event.preventDefault();
      event.stopPropagation();
    }

  }

  @Override
  protected void setChoiceRenderer(ChoiceRenderer<T> choiceRenderer) {
    if (choiceRenderer instanceof SelectRenderer) {
      this.selectRenderer = (SelectRenderer<T, U>) choiceRenderer;
    }
  }

  @Override
  protected ChoiceRenderer<T> getChoiceRenderer() {
    return selectRenderer;
  }

  @Override
  protected ChoiceSelectionHandler<T, U> getSelectionHandler() {
    assert selectionHandler != null : "SelectionHandler is not set !";
    return selectionHandler;
  }

  @Override
  protected void setSelectionHandler(ChoiceSelectionHandler<T, U> selectionHandler) {
    if (selectionHandler instanceof SelectItemSelectionHandler) {
      this.selectionHandler = (SelectItemSelectionHandler<T, U>) selectionHandler;
    }
  }

  protected Map<T, NavLink> getItemsLinks() {
    return itemsLinks;
  }

  protected SimpleDropdown getDropdown() {
    return dropdown;
  }

}
