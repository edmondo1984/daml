-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

ALTER TABLE participant_events SET (FILLFACTOR = 80);
ALTER TABLE participant_events SET (toast_tuple_target = 384);
