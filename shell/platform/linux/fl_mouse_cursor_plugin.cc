// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "flutter/shell/platform/linux/fl_mouse_cursor_plugin.h"

#include <gtk/gtk.h>
#include <cstring>

#include "flutter/shell/platform/linux/public/flutter_linux/fl_method_channel.h"
#include "flutter/shell/platform/linux/public/flutter_linux/fl_standard_method_codec.h"

static constexpr char kChannelName[] = "flutter/mousecursor";
static constexpr char kBadArgumentsError[] = "Bad Arguments";

static constexpr char kActivateSystemCursorMethod[] = "activateSystemCursor";
static constexpr char kKindKey[] = "kind";

static constexpr char kCreateImageCursorMethod[] = "createImageCursor";
static constexpr char kDataKey[] = "data";
static constexpr char kWidthKey[] = "width";
static constexpr char kHeightKey[] = "height";
static constexpr char kOffsetXKey[] = "offsetX";
static constexpr char kOffsetYKey[] = "offsetY";

static constexpr char kActivateImageCursorMethod[] = "activateImageCursor";
static constexpr char kCursorIdKey[] = "cursorId";

static constexpr char kFallbackCursor[] = "default";

struct _FlMouseCursorPlugin {
  GObject parent_instance;

  FlMethodChannel* channel;

  FlView* view;

  GHashTable* system_cursor_table;

  GUINT last_image_cursor_id;

  GHashTable* image_mouse_cursors;
};

G_DEFINE_TYPE(FlMouseCursorPlugin, fl_mouse_cursor_plugin, G_TYPE_OBJECT)

// Insert a new entry into a hashtable from strings to strings.
//
// Returns whether the newly added value was already in the hash table or not.
static bool define_system_cursor(GHashTable* table,
                                 const gchar* key,
                                 const gchar* value) {
  return g_hash_table_insert(
      table, reinterpret_cast<gpointer>(const_cast<gchar*>(key)),
      reinterpret_cast<gpointer>(const_cast<gchar*>(value)));
}

// Populate the hash table so that it maps from Flutter's cursor kinds to GTK's
// cursor values.
//
// The table must have been created as a hashtable from strings to strings.
static void populate_system_cursor_table(GHashTable* table) {
  // The following mapping must be kept in sync with Flutter framework's
  // mouse_cursor.dart.
  define_system_cursor(table, "alias", "alias");
  define_system_cursor(table, "allScroll", "all-scroll");
  define_system_cursor(table, "basic", "default");
  define_system_cursor(table, "cell", "cell");
  define_system_cursor(table, "click", "pointer");
  define_system_cursor(table, "contextMenu", "context-menu");
  define_system_cursor(table, "copy", "copy");
  define_system_cursor(table, "forbidden", "not-allowed");
  define_system_cursor(table, "grab", "grab");
  define_system_cursor(table, "grabbing", "grabbing");
  define_system_cursor(table, "help", "help");
  define_system_cursor(table, "move", "move");
  define_system_cursor(table, "none", "none");
  define_system_cursor(table, "noDrop", "no-drop");
  define_system_cursor(table, "precise", "crosshair");
  define_system_cursor(table, "progress", "progress");
  define_system_cursor(table, "text", "text");
  define_system_cursor(table, "resizeColumn", "col-resize");
  define_system_cursor(table, "resizeDown", "s-resize");
  define_system_cursor(table, "resizeDownLeft", "sw-resize");
  define_system_cursor(table, "resizeDownRight", "se-resize");
  define_system_cursor(table, "resizeLeft", "w-resize");
  define_system_cursor(table, "resizeLeftRight", "ew-resize");
  define_system_cursor(table, "resizeRight", "e-resize");
  define_system_cursor(table, "resizeRow", "row-resize");
  define_system_cursor(table, "resizeUp", "n-resize");
  define_system_cursor(table, "resizeUpDown", "ns-resize");
  define_system_cursor(table, "resizeUpLeft", "nw-resize");
  define_system_cursor(table, "resizeUpRight", "ne-resize");
  define_system_cursor(table, "resizeUpLeftDownRight", "nwse-resize");
  define_system_cursor(table, "resizeUpRightDownLeft", "nesw-resize");
  define_system_cursor(table, "verticalText", "vertical-text");
  define_system_cursor(table, "wait", "wait");
  define_system_cursor(table, "zoomIn", "zoom-in");
  define_system_cursor(table, "zoomOut", "zoom-out");
}

// Sets the mouse cursor as a system cursor.
FlMethodResponse* activate_system_cursor(FlMouseCursorPlugin* self,
                                         FlValue* args) {
  if (fl_value_get_type(args) != FL_VALUE_TYPE_MAP) {
    return FL_METHOD_RESPONSE(fl_method_error_response_new(
        kBadArgumentsError, "Argument map missing or malformed", nullptr));
  }

  FlValue* kind_value = fl_value_lookup_string(args, kKindKey);
  if (fl_value_get_type(kind_value) != FL_VALUE_TYPE_STRING) {
    return FL_METHOD_RESPONSE(fl_method_error_response_new(
        kBadArgumentsError, "Argument is malformed.", nullptr));
  }
  const gchar* kind = fl_value_get_string(kind_value);

  if (self->system_cursor_table == nullptr) {
    self->system_cursor_table = g_hash_table_new(g_str_hash, g_str_equal);
    populate_system_cursor_table(self->system_cursor_table);
  }

  const gchar* cursor_name = reinterpret_cast<const gchar*>(
      g_hash_table_lookup(self->system_cursor_table, kind));
  if (cursor_name == nullptr) {
    cursor_name = kFallbackCursor;
  }

  GdkWindow* window =
      gtk_widget_get_window(gtk_widget_get_toplevel(GTK_WIDGET(self->view)));
  g_autoptr(GdkCursor) cursor =
      gdk_cursor_new_from_name(gdk_window_get_display(window), cursor_name);
  gdk_window_set_cursor(window, cursor);

  return FL_METHOD_RESPONSE(fl_method_success_response_new(nullptr));
}

FlMethodResponse* create_image_cursor(FlMouseCursorPlugin* self,
                                         FlValue* args) {
  if (fl_value_get_type(args) != FL_VALUE_TYPE_MAP) {
    return FL_METHOD_RESPONSE(fl_method_error_response_new(
        kBadArgumentsError, "Argument is missing or not a map", nullptr));
  }

  FlValue* data_value = fl_value_lookup_string(args, kDataKey);
  FlValue* width_value = fl_value_lookup_string(args, kWidthKey);
  FlValue* height_value = fl_value_lookup_string(args, kHeightKey);
  FlValue* offset_x_value = fl_value_lookup_string(args, kOffsetXKey);
  FlValue* offset_y_value = fl_value_lookup_string(args, kOffsetYKey);
  if (fl_value_get_type(data_value) != FL_VALUE_TYPE_UINT8_LIST ||
      fl_value_get_type(width_value) != FL_VALUE_TYPE_INT ||
      fl_value_get_type(height_value) != FL_VALUE_TYPE_INT ||
      fl_value_get_type(offset_x_value) != FL_VALUE_TYPE_INT ||
      fl_value_get_type(offset_y_value) != FL_VALUE_TYPE_INT) {
    return FL_METHOD_RESPONSE(fl_method_error_response_new(
        kBadArgumentsError, "Argument is malformed.", nullptr));
  }

  const uint8_t* data = fl_value_get_uint8_list(data_value);
  const int width = fl_value_get_int(width_value);
  const int height = fl_value_get_int(height_value);
  const int offset_x = fl_value_get_int(offset_x_value);
  const int offset_y = fl_value_get_int(offset_y_value);

  gdk_cursor_new_from_pixbuf();

  GdkWindow* window =
      gtk_widget_get_window(gtk_widget_get_toplevel(GTK_WIDGET(self->view)));
  g_autoptr(GdkPixbuf) pixbuf = gdk_pixbuf_new_from_data(
    data,
    GDK_COLORSPACE_RGB,
    TRUE,
    8,
    width,
    height,
    width * 4,
    NULL,
    NULL);
  GdkCursor* cursor =
      gdk_cursor_new_from_pixbuf(
        gdk_window_get_display(window),
        pixbuf,
        offset_x,
        offset_y);
  self->last_image_cursor_id += 1;
  g_hash_table_insert(
      self->image_mouse_cursors, GUINT_TO_POINTER(self->last_image_cursor_id),
      cursor);

  return FL_METHOD_RESPONSE(fl_method_success_response_new(fl_value_new_int(self->last_image_cursor_id)));
}

// Sets the mouse cursor as an image cursor.
FlMethodResponse* activate_image_cursor(FlMouseCursorPlugin* self,
                                         FlValue* args) {
  if (fl_value_get_type(args) != FL_VALUE_TYPE_MAP) {
    return FL_METHOD_RESPONSE(fl_method_error_response_new(
        kBadArgumentsError, "Argument map missing or malformed", nullptr));
  }

  FlValue* cursor_id_value = fl_value_lookup_string(args, kCursorIdKey);
  if (fl_value_get_type(cursor_id_value) != FL_VALUE_TYPE_INT) {
    return FL_METHOD_RESPONSE(fl_method_error_response_new(
        kBadArgumentsError, "Argument is malformed.", nullptr));
  }
  const GUINT cursor_id = (GUINT)fl_value_get_int(cursor_id_value);

  GdkCursor* cursor = reinterpret_cast<GdkCursor*>(
      g_hash_table_lookup(self->image_mouse_cursors, GUINT_TO_POINTER(cursor_id)));

  GdkWindow* window =
      gtk_widget_get_window(gtk_widget_get_toplevel(GTK_WIDGET(self->view)));
  gdk_window_set_cursor(window, cursor);

  return FL_METHOD_RESPONSE(fl_method_success_response_new(nullptr));
}

// Called when a method call is received from Flutter.
static void method_call_cb(FlMethodChannel* channel,
                           FlMethodCall* method_call,
                           gpointer user_data) {
  FlMouseCursorPlugin* self = FL_MOUSE_CURSOR_PLUGIN(user_data);

  const gchar* method = fl_method_call_get_name(method_call);
  FlValue* args = fl_method_call_get_args(method_call);

  g_autoptr(FlMethodResponse) response = nullptr;
  if (strcmp(method, kActivateSystemCursorMethod) == 0) {
    response = activate_system_cursor(self, args);
  } else if (strcmp(method, kCreateImageCursorMethod) == 0) {
    response = create_image_cursor(self, args);
  } else if (strcmp(method, kActivateImageCursorMethod) == 0) {
    response = activate_image_cursor(self, args);
  } else {
    response = FL_METHOD_RESPONSE(fl_method_not_implemented_response_new());
  }

  g_autoptr(GError) error = nullptr;
  if (!fl_method_call_respond(method_call, response, &error)) {
    g_warning("Failed to send method call response: %s", error->message);
  }
}

static void fl_mouse_cursor_plugin_dispose(GObject* object) {
  FlMouseCursorPlugin* self = FL_MOUSE_CURSOR_PLUGIN(object);

  g_clear_object(&self->channel);
  if (self->view != nullptr) {
    g_object_remove_weak_pointer(G_OBJECT(self->view),
                                 reinterpret_cast<gpointer*>(&(self->view)));
    self->view = nullptr;
  }
  g_clear_pointer(&self->system_cursor_table, g_hash_table_unref);

  G_OBJECT_CLASS(fl_mouse_cursor_plugin_parent_class)->dispose(object);
}

static void fl_mouse_cursor_plugin_class_init(FlMouseCursorPluginClass* klass) {
  G_OBJECT_CLASS(klass)->dispose = fl_mouse_cursor_plugin_dispose;
}

static void fl_mouse_cursor_plugin_init(FlMouseCursorPlugin* self) {}

FlMouseCursorPlugin* fl_mouse_cursor_plugin_new(FlBinaryMessenger* messenger,
                                                FlView* view) {
  g_return_val_if_fail(FL_IS_BINARY_MESSENGER(messenger), nullptr);

  FlMouseCursorPlugin* self = FL_MOUSE_CURSOR_PLUGIN(
      g_object_new(fl_mouse_cursor_plugin_get_type(), nullptr));

  g_autoptr(FlStandardMethodCodec) codec = fl_standard_method_codec_new();
  self->channel =
      fl_method_channel_new(messenger, kChannelName, FL_METHOD_CODEC(codec));
  fl_method_channel_set_method_call_handler(self->channel, method_call_cb, self,
                                            nullptr);
  self->view = view;
  if (view != nullptr) {
    g_object_add_weak_pointer(G_OBJECT(view),
                              reinterpret_cast<gpointer*>(&(self->view)));
  }

  self->image_mouse_cursors = g_hash_table_new(g_direct_hash, g_direct_equal);
  self->last_image_cursor_id = 1;

  return self;
}
